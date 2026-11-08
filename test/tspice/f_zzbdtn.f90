!$Procedure F_ZZBDTN ( ZZBODTRN Test Family  )

      SUBROUTINE F_ZZBDTN ( OK )

! $ Abstract
!
!     Test family to exercise the logic and code in ZZBODTRN and its
!     entry points.
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

      INCLUDE              'zzbodtrn.inc'
      INCLUDE              'zzctr.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicated test status.
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
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine exercises the entry points of ZZBODTRN.
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
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 25-APR-2014 (BVS)
!
!        Updated ZZBODTRN call to match new calling sequence.
!
!-    TSPICE Version 1.0.0, 21-AUG-2002 (FST)
!
!
!-&

!
!     Local Parameters
!
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 200 )

!
!     Local Variables
!
      CHARACTER*(LINLEN)    BUFFER ( 4 )
      CHARACTER*(NAMLEN)    NAME

      INTEGER               CODE
      INTEGER               USRCTR ( CTRSIZ )

      LOGICAL               FOUND
      LOGICAL               UPDATE

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZBDTN' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZBODTRN - SPICE(BOGUSENTRY) exception' )

      CALL ZZBODTRN ( NAME, CODE, FOUND, USRCTR, UPDATE )

      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZBODRST - Coverage Test' )

!
!     Install a new mapping.
!
      CALL ZZBODDEF ( 'NAME', 1000 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check that the mapping is installed.
!
      CALL ZZBODN2C ( 'NAME', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 1000, 0, OK )

!
!     Reset the body name-code system.
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
!     Now verify that NAME<->1000 is not currently assigned.
!
      CALL ZZBODN2C ( 'NAME', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL ZZBODC2N ( 1000, 'NAME', FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZBODTRN - Check watchers are properly set' )

!
!     Make certain that the initialization block is executed
!     prior to attempting to check the watcher's set status.
!     It is difficult to verify that each initialization block
!     functions properly in a test family, since once INIT is
!     set the code will not execute again.
!
      CALL ZZBODN2C ( 'EARTH', CODE, FOUND )

      BUFFER(  1 ) = 'NAIF_BODY_CODE = 3000'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''ZZBODTRN_TEST_BODY'''

      CALL LMPOOL ( BUFFER, 2 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check to see that the watchers are activated.
!
      CALL CVPOOL ( 'ZZBODTRN', UPDATE )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'UPDATE', UPDATE, .TRUE., OK )

!
!     Clean up.
!
      CALL CLPOOL ( )
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZBODN2C - Coverage Test - UPDATE' )

!
!     Cause CVPOOL to set UPDATE to .TRUE.
!
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 3000'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''ZZBODTRN_TEST_BODY_2' &
      //             ''''

      CALL LMPOOL ( BUFFER, 2 )

      CODE  = 0
      FOUND = .TRUE.

!
!     Now invoke the module with an unknown name.
!
      CALL ZZBODN2C ( '<UNDEFINED>', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )

!
!     Verify that ZZBODTRN_TEST_BODY_2 is available now. EXTKER
!     success path.
!
      CALL LMPOOL ( BUFFER, 2 )

      CALL ZZBODN2C ( 'ZZBODTRN_TEST_BODY_2', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 3000, 0, OK )

!
!     Check a built-in assignment, EXTKER failure path.  Make LJUST
!     UCASE, and COMPRSS perform non-trivial operations.
!
      CALL LMPOOL ( BUFFER, 2 )

      CALL ZZBODN2C ( '    earth    barycenter', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 3, 0, OK )

!
!     Set UPDATE and force EXTKER to be .FALSE., look for unknown name.
!
      CALL CLPOOL ( )

      CODE  = 0
      FOUND = .TRUE.

      CALL ZZBODN2C ( '<UNDEFINED>', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )

!
!     Check to see that ZZBODTRN_TEST_BODY_2 is no longer available.
!
      CALL CLPOOL ( )

      CODE  = 0
      FOUND = .TRUE.

      CALL ZZBODN2C ( 'ZZBODTRN_TEST_BODY_2', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )

!
!     Lastly look up a built-in name's code.
!
      CALL CLPOOL ( )

      CALL ZZBODN2C ( 'EARTH', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 399, 0, OK )

      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZBODN2C - Coverage Test - no UPDATE' )

!
!     Force EXTKER to be set.
!
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 3000'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''ZZBODTRN_TEST_BODY_3' &
      //             ''''

      CALL LMPOOL ( BUFFER, 2 )

!
!     Make sure no watchers set UPDATE.
!
      CODE = 0

      CALL ZZBODN2C ( '<UNDEFINED>', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )

!
!     Now invoke ZZBODN2C with the name defined in the kernel pool.
!
      CALL ZZBODN2C ( 'ZZBODTRN_TEST_BODY_3', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 3000, 0, OK )

!
!     Now invoke ZZBODN2C with a built-in name.
!
      CALL ZZBODN2C ( 'EARTH', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 399, 0, OK )

!
!     Lastly invoke it with an unknown name.
!
      CODE = 0

      CALL ZZBODN2C ( '<UNDEFINED>', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )

!
!     Clear the pool and remove EXTKER's .TRUE. status.
!
      CALL CLPOOL ( )

      CODE = 0

      CALL ZZBODN2C ( '<UNDEFINED>', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )

!
!     Now check the previously defined name in the kernel pool.
!
      CODE = 0

      CALL ZZBODN2C ( 'ZZBODTRN_TEST_BODY_3', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )

!
!     Check a built-in code.
!
      CALL ZZBODN2C ( 'EARTH', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 399, 0, OK )

!
!     Lastly invoke it with an undefined name.
!
      CODE = 0

      CALL ZZBODN2C ( '<UNDEFINED>', CODE, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 0, 0, OK )

      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZBODC2N - Coverage Test - UPDATE' )

!
!     Cause CVPOOL to set UPDATE to .TRUE.
!
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 3000'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''ZZBODTRN_TEST_BODY_4' &
      //             ''''
      BUFFER(  3 ) = 'NAIF_BODY_CODE += 3002'
      BUFFER(  4 ) = 'NAIF_BODY_NAME += ''EARTH'''

      CALL LMPOOL ( BUFFER, 4 )

      NAME = '<UNCHANGED>'
      FOUND = .TRUE.

!
!     Now invoke the module with an unknown code.
!
      CALL ZZBODC2N ( 3001, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Verify that 3001 is available now. EXTKER success path.
!
      CALL LMPOOL ( BUFFER, 4 )

      CALL ZZBODC2N ( 3000, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'ZZBODTRN_TEST_BODY_4', OK )

!
!     Check a built-in assignment, EXTKER failure path no "kernel
!     pool" masking.
!
      CALL LMPOOL ( BUFFER, 4 )

      CALL ZZBODC2N ( 3, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'EARTH BARYCENTER', OK )

!
!     Check a built-in assignment, EXTKER failure path with "kernel
!     pool" masking.
!
      CALL LMPOOL ( BUFFER, 4 )

      NAME = '<UNCHANGED>'

      CALL ZZBODC2N ( 399, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Set UPDATE and force EXTKER to be .FALSE., look for unknown code.
!
      CALL CLPOOL ( )

      NAME  = '<UNCHANGED>'
      FOUND = .TRUE.

      CALL ZZBODC2N ( 3001, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Check to see that 3000 is no longer available.
!
      CALL CLPOOL ( )

      NAME  = '<UNCHANGED>'
      FOUND = .TRUE.

      CALL ZZBODC2N ( 3000, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Lastly look up a built-in code's name.
!
      CALL CLPOOL ( )

      CALL ZZBODC2N ( 399, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'EARTH', OK )

      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZBODC2N - Coverage Test - no UPDATE' )

!
!     Force EXTKER to be set.
!
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 3000'
      BUFFER(  2 ) = 'NAIF_BODY_NAME = ''ZZBODTRN_TEST_BODY_5' &
      //             ''''
      BUFFER(  3 ) = 'NAIF_BODY_CODE += 3002'
      BUFFER(  4 ) = 'NAIF_BODY_NAME += ''EARTH'''

      CALL LMPOOL ( BUFFER, 4 )

!
!     Make sure no watchers set UPDATE.
!
      NAME = '<UNCHANGED>'

      CALL ZZBODC2N ( 3001, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Now invoke ZZBODN2C with a code defined in the kernel pool.
!
      CALL ZZBODC2N ( 3000, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'ZZBODTRN_TEST_BODY_5', OK )

!
!     Check a built-in code, EXTKER is .TRUE., with no "kernel pool"
!     masking.
!
      CALL ZZBODC2N ( 3, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'EARTH BARYCENTER', OK )


!
!     Check a built-in code, EXTER is .TRUE., with "kernel pool"
!     masking.
!
      NAME = '<UNCHANGED>'

      CALL ZZBODC2N ( 399, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Check for an unknown code.
!
      NAME = '<UNCHANGED>'

      CALL ZZBODC2N ( 3001, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Now clear the pool to remove EXTKER's .TRUE. status.
!
      CALL CLPOOL ( )

      NAME = '<UNCHANGED>'

!
!     Look up an unknown code, to clear UPDATE's status.
!
      CALL ZZBODC2N ( 3001, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

!
!     Now check the previously defined codes in the kernel pool.
!
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

!
!     Now look up the built-in codes.
!
      CALL ZZBODC2N ( 399, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'EARTH', OK )

      CALL ZZBODC2N ( 3, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', 'EARTH BARYCENTER', OK )

!
!     Lastly invoke it with an unknown code.
!
      NAME = '<UNCHANGED>'

      CALL ZZBODC2N ( 3001, NAME, FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )
      CALL CHCKSC ( 'NAME', NAME, '=', '<UNCHANGED>', OK )

      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZBODDEF - Exception SPICE(BLANKNAMEASSIGNED)' )

      CALL ZZBODDEF ( ' ', 1000 )

      CALL CHCKXC ( .TRUE., 'SPICE(BLANKNAMEASSIGNED)', OK )

!
!     Check that it did nothing to the name-code mapping
!     system.
!
      CALL ZZBODN2C ( ' ', CODE, FOUND )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

      CALL ZZBODC2N ( 1000, NAME, FOUND )

      CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

!
!     Clean up.
!
      CALL ZZBODRST ( )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZBODDEF - Coverage Tests - Name Replace Case' )

!
!     Replace the name for EARTH with the same code.  This forces a
!     simple replace, rather than a sort/update.
!
      CALL ZZBODDEF ( 'eArTh', 399 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     See if it worked.
!
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

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZBODDEF - Coverage Tests - Name Replace Sort' )

!
!     Replace the name for EARTH with a new code.  This forces a
!     compression and append operation.
!
      CALL ZZBODDEF ( 'EaRtH', 1000 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     See if it worked.
!
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

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZBODDEF - Coverage Tests - Append New' )

!
!     Replace the name for EARTH with a new code.  This forces a
!     compression and append operation.
!
      CALL ZZBODDEF ( 'SPUDSpam', 1000 )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     See if it worked.
!
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

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
