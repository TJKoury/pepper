!$Procedure F_ZZBDTRN ( Family of tests for ZZBODTRN )

      SUBROUTINE F_ZZBDTRN ( OK )
      use pepper_inc_zzbodtrn

! $ Abstract
!
!     This family of tests exercises the ZZBODTRN interfaces
!     BODN2C, BODC2N and BODDEF.
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
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the entry points of ZZBODTRN.
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
!     N.J. Bachman     (JPL)
!     E.D. Wright      (JPL)
!
! $ Version
!
!     TSPICE Version 3.1.0  06-MAR-2009 (NJB)
!
!        Added test cases for handling of interaction
!        of watchers and kernel pool look-up failures.
!
!        Completed module header.
!
!-    Version 3.0.0  22-APR-2003 (EDW)
!
!        Testing of the default SPICE body list removed.
!        Those tests now exist in F_ZZBDTRN1.
!
!        This test family now examines precedence with regards
!        to masking.
!
!-    Version 2.4.0  21-FEB-2003 (BVS)
!
!        Changed MER-A back to MER-1. Added test for MER-2.
!
!-    Version 2.3.0  04-DEC-2002 (EDW)
!
!        Added new assignments from the default collection
!        to the test arrays ID and NAME:
!
!       -226     ROSETTA
!        517     CALLIRRHOE
!        518     THEMISTO
!        519     MAGACLITE
!        520     TAYGETE
!        521     CHALDENE
!        522     HARPALYKE
!        523     KALYKE
!        524     IOCASTE
!        525     ERINOME
!        526     ISONOE
!        527     PRAXIDIKE
!
!        These assignments also required a reset of the
!        NPAIR parameter to 308.
!
!-    Version 2.2.1  14-AUG-2002 (EDW)
!
!        Altered the string "MER-1" to "MER-A" to reflect the
!        name change in zzbodtrn.f
!
!        Removed BADMEMBERREASSIGN tests as such errors no longer
!        exist. Added first masking tests.
!
!-    Version 2.2.0  17-OCT-2001 (EDW)
!
!        Added test for BODDEF, BODC2N string in equals
!        string out functionality. Reassign failure tests
!        modified to BADMEMBERREASSIGN.
!
!        Additional test to ensure functionality of BODTRN routines
!        unaffected after a error condition.
!
!        Removed redundant name/body pairs in test list.
!
!-    Version 2.1.0  24-APR-2001 (EDW)
!
!        Added test to check a name listed in the
!        NAIF_BODY_NAME kernel variable is not reassigned
!        to a new ID code. Technically, the test operates on
!        both the kernel variable and the default name-ID
!        list.
!
!        Added test to insure case insensitivity between a
!        name to code lookup request and a name in the kernel pool.
!
!-    Version 2.0.0  14 OCT 1999 (WLT)
!
!        Removed a call to BODN2C that passed a number instead
!        of a string.
!-&


!
!     Test Utility Functions
!
      CHARACTER*(10)           BEGDAT

!
!     SPICELIB Functions
!


!
!     Local Variables
!

      INTEGER                  LNSIZE
      PARAMETER              ( LNSIZE = 80 )

      CHARACTER*( LNSIZE )     TEXT ( 6 )

      CHARACTER*(MAXL)         GET
      INTEGER                  CODE
      LOGICAL                  FOUND


!
!     Begin every test family with an open call.
!

      CALL TOPEN ( 'F_ZZBDTRN' )


!
!     CASE 1
!
      CALL TCASE ( 'Checking to make sure FOUND comes back FALSE ' &
      //           'for a bogus name. ' )

      CALL BODN2C ( 'SPUD', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )


!
!     CASE 2
!
      CALL TCASE ( 'Checking to make sure that FOUND comes back ' &
      //           'FALSE for a bogus ID code.' )

      CALL BODC2N ( 22, GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )

!
!     CASE 3
!
      CALL TCASE ( 'Checking to make sure we can load a kernel ' &
      //           'and add new ID/NAME pairs. ' )


      TEXT(  1 ) = BEGDAT()
      TEXT(  2 ) = ' NAIF_BODY_CODE  = ( 22, 23, 24, 25 )'
      TEXT(  3 ) = ' NAIF_BODY_NAME  = ( ''SPUD'', ''MOON''' &
         //           ', ''CURLEY'', ''SHEMP'' )'

      CALL TSTTXT ( 'sample.nam', TEXT, 3, .TRUE., .FALSE.   )

      CALL BODN2C ( 'SPUD', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 22, 0, OK )

      CALL BODC2N ( 24, GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,           OK )
      CALL CHCKSC ( 'GET'  , GET  , '='   , 'CURLEY', OK )

!
!     CASE 4
!
!     Test to check we can override a name-ID assignment from the
!     default list with an external kernel assignment.
!
      CALL TCASE ( 'Checking to make sure we can override an ' &
      //           'existing NAME/ID pair. ' )

      CALL BODN2C ( 'MOON', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 23, 0, OK )


!
!     CASE 5
!
!     Test on external name-ID assignments.
!
      CALL TCASE ( 'Check to ensure case insensitivity in N2C.' )

      TEXT(1) = BEGDAT()
      TEXT(2) = 'NAIF_BODY_CODE =  ( -170101, -170102 )'
      TEXT(3) = 'NAIF_BODY_NAME =  ( ''1701-A'', ''   1701-b'' )'
      TEXT(4) = ' '

      CALL KILFIL ( 'testdata.ker' )
      CALL TSTTXT ( 'testdata.ker', TEXT, 4, .FALSE., .TRUE.)
      CALL LDPOOL ( 'testdata.ker' )

      CALL BODN2C ( '1701-a', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', -170101, 0, OK )

      CALL BODN2C ( ' 1701-B ', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', -170102, 0, OK )

      CALL BODC2N ( -170102, GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSC ( 'GET', GET, '=', '   1701-b', OK )

!
!     CASE 6
!
      CALL TCASE ( 'Checking that we cannot override an ' &
      //           'existing kernel based NAME/ID pair.' )

      TEXT(1) = BEGDAT()
      TEXT(2) = 'NAIF_BODY_CODE =  ( -170101, -170102 )'
      TEXT(3) = 'NAIF_BODY_NAME =  ( ''1701-A'', ''1701-b'' )'
      TEXT(4) = 'NAIF_BODY_CODE += ( -170105 )'
      TEXT(5) = 'NAIF_BODY_NAME += ( ''1701-a'' )'
      TEXT(6) = ' '

      CALL KILFIL ( 'testdata.ker' )
      CALL TSTTXT ( 'testdata.ker', TEXT, 6, .FALSE., .TRUE.)

!
!     CASE 7
!
!     Test of BODDEF.
!
      CALL TCASE ( 'Check BODDEF functionality.' )

      CALL BODDEF ( 'Woof', -10009 )

      CALL BODC2N ( -10009, GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSC ( 'GET', GET, '=', 'Woof', OK )

!
!     Assign a member of the equivalence class to the ID code.
!     As last on the stack, a C2N lookup should return the
!     same member string.
!
      CALL BODDEF ( 'woof', -10009 )
      CALL BODC2N ( -10009, GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSC ( 'GET', GET, '=', 'woof', OK )

!
!     Case and space insensitiviy
!
      CALL BODN2C ( ' WOOF ', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', -10009, 0, OK )


!
!     Functionality unaffected by the previous test's error state?
!     Try a BODDEF.
!
      CALL BODDEF ( ' WOof ', -10009 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

!
!     Now perform a post error case and space insensitiviy test.
!
      CALL BODN2C ( '   wOOf ', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', -10009, 0, OK )


!
!     CASE 8
!
!     Test the mapping precedence behavior.
!
!     Assign a mapping via BODDEF. Note: 'Tis very important to call
!     CLPOOL an clear the kernel pool before this set of tests. The
!     previous tests left the pool in an error signalling state.
!
      CALL CLPOOL ()

      CALL TCASE ( 'Check precedence functionality. BODDEF 1' )

      CALL BODDEF ( 'TestBODEF1', -30001 )

      GET = ' '
      CALL BODC2N ( -30001,   GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSC ( 'GET', GET, '=', 'TestBODEF1', OK )

      CALL BODN2C ( 'TestBODEF1', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', -30001, 0, OK )

!
!     CASE 9
!
!     Load a new mapping to the kernel pool using the previous ID value.
!     This mapping should override BODDEF.
!
      CALL TCASE ( 'Check precedence functionality. TestKer 1' )

      TEXT(  1 ) = '      NAIF_BODY_NAME += ''TestKer1'''
      TEXT(  2 ) = '      NAIF_BODY_CODE += -30001'

      CALL LMPOOL ( TEXT, 2 )

      GET = ' '
      CALL BODC2N ( -30001,   GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSC ( 'GET', GET, '=', 'TestKer1', OK )


      CALL BODN2C ( 'TestKer1', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', -30001, 0, OK )


!
!     CASE 10
!
!     Assign another name to the previous ID code. This mapping
!     should override all others.
!
      CALL TCASE ( 'Check precedence functionality. TestKer 2' )

      TEXT(  1 ) = '      NAIF_BODY_NAME += ''TestKer2'''
      TEXT(  2 ) = '      NAIF_BODY_CODE += -30001'

      CALL LMPOOL ( TEXT, 2 )

      GET = ' '
      CALL BODC2N ( -30001,   GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSC ( 'GET', GET, '=', 'TestKer2', OK )

      CALL BODN2C ( 'TestKer2', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', -30001, 0, OK )


!
!     CASE 11
!
!     Clear the kernel pool, test for BODDEF's first mapping for the
!     test ID.
!
      CALL TCASE ( 'Check precedence functionality. BODDEF 1 retest.' )

      CALL CLPOOL()

      GET = ' '
      CALL BODC2N ( -30001,   GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSC ( 'GET', GET, '=', 'TestBODEF1', OK )


      CALL BODN2C ( 'TestBODEF1', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', -30001, 0, OK )


!
!     CASE 12
!
!     Assign another mapping via BODDEF to override the first
!     BODDEF mapping.
!
      CALL TCASE ( 'Check precedence functionality. BODDEF 2.' )

      CALL BODDEF ( 'TestBODEF2', -30001 )

      GET = ' '
      CALL BODC2N ( -30001,   GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSC ( 'GET', GET, '=', 'TestBODEF2', OK )

      CALL BODN2C ( 'TestBODEF2', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', -30001, 0, OK )


!
!     CASE 13
!
      CALL TCASE ( 'Check precedence functionality. Masking.' )

      CALL CLPOOL ()

!
!     Assign a mapping via BODDEF, then remap the name to a new
!     number.
!
      CALL BODDEF ( 'TestName', 20000 )
      TEXT(  1 ) = '      NAIF_BODY_NAME = ''TestName'''
      TEXT(  2 ) = '      NAIF_BODY_CODE = -20000'

      CALL LMPOOL ( TEXT, 2 )

      CALL BODN2C ( 'TestName', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', -20000, 0, OK )

      GET = ' '
      CALL BODC2N ( -20000,   GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSC ( 'GET', GET, '=', 'TestName', OK )


!
!     CASE 14
!
!
!     The kernel pool assignment should mask the BODDEF assignment
!     so a request for the name of body 20000 ought to fail.
!
      CALL TCASE ( 'Check precedence functionality. De-masking.' )

      GET = ' '
      CALL BODC2N ( 20000,   GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )

!
!     Destroy the kernel pool entries. The BODDEF assignment should now
!     respond; a request for body 20000 should succeed.
!
      CALL CLPOOL ( )
      GET = ' '
      CALL BODC2N ( 20000,   GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSC ( 'GET', GET, '=', 'TestName', OK )


!
!     CASE 15
!
!     Set the kernel assignment using a different body ID than
!     previous tests. This should remask the BODDEF assignment.
!

      CALL TCASE ( 'Check precedence functionality. Re-masking.' )

      TEXT(  1 ) = '      NAIF_BODY_NAME = ''TestName'''
      TEXT(  2 ) = '      NAIF_BODY_CODE = -30000'
      CALL LMPOOL ( TEXT, 2 )

      CALL BODN2C ( 'TestName', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', -30000, 0, OK )

      GET = ' '
      CALL BODC2N ( -30000,   GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSC ( 'GET', GET, '=', 'TestName', OK )


!
!     CASE 16
!
!     Test watcher logic in ZZBODN2C: if ZZBODKER fails due
!     to a body-name assignment error, ZZBODN2C should exit.
!     Make the call twice to ensure correct handling of the
!     error.
!
      CALL TCASE ( 'ZZBODN2C: test CVPOOL handling for bad mapping ' )

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ',  OK )

      TEXT(  1 ) = BEGDAT()
      TEXT(  2 ) = ' NAIF_BODY_CODE  = ( 22, 23, 24, 25 )'
      TEXT(  3 ) = ' NAIF_BODY_NAME  = ( ''SPUD'', ''MOON'',' &
         //                            '  ''CURLEY'' )'

      CALL TSTTXT ( 'sample.nam', TEXT, 3, .TRUE., .FALSE.   )

      CALL ZZBODN2C ( 'MOON', CODE, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSIONS)',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )


      CALL ZZBODN2C ( 'MOON', CODE, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSIONS)',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )


!
!     CASE 17
!
!     Test watcher logic in ZZBODC2N: if ZZBODKER fails due
!     to a body-name assignment error, ZZBODC2N should exit.
!     Make the call twice to ensure correct handling of the
!     error.
!
      CALL TCASE ( 'ZZBODC2N: test CVPOOL handling for bad mapping ' )

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ',  OK )

      TEXT(  1 ) = BEGDAT()
      TEXT(  2 ) = ' NAIF_BODY_CODE  = ( 22, 23, 24, 25 )'
      TEXT(  3 ) = ' NAIF_BODY_NAME  = ( ''SPUD'', ''MOON'',' &
         //                            ' ''CURLEY'' )'

      CALL TSTTXT ( 'sample.nam', TEXT, 3, .TRUE., .FALSE.   )

      CALL ZZBODC2N ( 23, GET, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSIONS)',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )

      CALL ZZBODC2N ( 23, GET, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSIONS)',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )




!
!     CASE 17
!
!     Test watcher logic in ZZBODKIK: if ZZBODKER fails due
!     to a body-name assignment error, ZZBODKIK should exit.
!     Since ZZBODKIK doesn't make direct use of the kernel
!     pool assignments, call ZZBODC2N to ensure that it calls
!     ZZBODKER if NODATA is .TRUE. upon entry.
!
!     Run the sequence of calls twice to ensure correct handling of the
!     error.
!
      CALL TCASE ( 'ZZBODKIK: test CVPOOL handling for bad mapping ' )

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ',  OK )

      TEXT(  1 ) = BEGDAT()
      TEXT(  2 ) = ' NAIF_BODY_CODE  = ( 22, 23, 24, 25 )'
      TEXT(  3 ) = ' NAIF_BODY_NAME  = ( ''SPUD'', ''MOON'',' &
         //                            ' ''CURLEY'' )'

      CALL TSTTXT ( 'sample.nam', TEXT, 3, .TRUE., .FALSE.   )

      CALL ZZBODKIK
      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSIONS)',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )

      CALL ZZBODC2N ( 23, GET, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSIONS)',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )

      CALL ZZBODKIK
      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSIONS)',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )

      CALL ZZBODC2N ( 23, GET, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSIONS)',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )




      CALL T_SUCCESS ( OK )
      RETURN
      END
