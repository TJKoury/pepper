C$Procedure F_ZZBDTRN ( Family of tests for ZZBODTRN )
 
      SUBROUTINE F_ZZBDTRN ( OK )  

C$ Abstract
C
C     This family of tests exercises the ZZBODTRN interfaces
C     BODN2C, BODC2N and BODDEF.
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

      INCLUDE               'zzbodtrn.inc'

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
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the entry points of ZZBODTRN.
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
C     N.J. Bachman     (JPL)
C     E.D. Wright      (JPL)
C
C$ Version
C
C     TSPICE Version 3.1.0  06-MAR-2009 (NJB)
C
C        Added test cases for handling of interaction 
C        of watchers and kernel pool look-up failures.
C
C        Completed module header.
C
C-    Version 3.0.0  22-APR-2003 (EDW)
C
C        Testing of the default SPICE body list removed.
C        Those tests now exist in F_ZZBDTRN1.
C
C        This test family now examines precedence with regards
C        to masking.
C
C-    Version 2.4.0  21-FEB-2003 (BVS)
C
C        Changed MER-A back to MER-1. Added test for MER-2.
C
C-    Version 2.3.0  04-DEC-2002 (EDW)
C
C        Added new assignments from the default collection
C        to the test arrays ID and NAME:
C
C       -226     ROSETTA
C        517     CALLIRRHOE
C        518     THEMISTO
C        519     MAGACLITE
C        520     TAYGETE
C        521     CHALDENE
C        522     HARPALYKE
C        523     KALYKE
C        524     IOCASTE
C        525     ERINOME
C        526     ISONOE
C        527     PRAXIDIKE
C
C        These assignments also required a reset of the 
C        NPAIR parameter to 308.
C
C-    Version 2.2.1  14-AUG-2002 (EDW)
C
C        Altered the string "MER-1" to "MER-A" to reflect the
C        name change in zzbodtrn.f
C
C        Removed BADMEMBERREASSIGN tests as such errors no longer
C        exist. Added first masking tests.
C
C-    Version 2.2.0  17-OCT-2001 (EDW)
C
C        Added test for BODDEF, BODC2N string in equals
C        string out functionality. Reassign failure tests
C        modified to BADMEMBERREASSIGN.
C
C        Additional test to ensure functionality of BODTRN routines
C        unaffected after a error condition.
C
C        Removed redundant name/body pairs in test list.
C     
C-    Version 2.1.0  24-APR-2001 (EDW)
C
C        Added test to check a name listed in the
C        NAIF_BODY_NAME kernel variable is not reassigned
C        to a new ID code. Technically, the test operates on
C        both the kernel variable and the default name-ID
C        list.
C
C        Added test to insure case insensitivity between a 
C        name to code lookup request and a name in the kernel pool.
C
C-    Version 2.0.0  14 OCT 1999 (WLT)
C
C        Removed a call to BODN2C that passed a number instead
C        of a string.
C-&
 
 
C
C     Test Utility Functions
C
      CHARACTER*(10)           BEGDAT
 
C
C     SPICELIB Functions
C
 
 
C
C     Local Variables
C
 
      INTEGER                  LNSIZE
      PARAMETER              ( LNSIZE = 80 )
 
      CHARACTER*( LNSIZE )     TEXT ( 6 )
 
      CHARACTER*(MAXL)         GET
      INTEGER                  CODE
      LOGICAL                  FOUND


C
C     Begin every test family with an open call.
C
 
      CALL TOPEN ( 'F_ZZBDTRN' )


C
C     CASE 1
C
      CALL TCASE ( 'Checking to make sure FOUND comes back FALSE '
     .//           'for a bogus name. ' )
 
      CALL BODN2C ( 'SPUD', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
 

C
C     CASE 2
C
      CALL TCASE ( 'Checking to make sure that FOUND comes back '
     .//           'FALSE for a bogus ID code.' )
 
      CALL BODC2N ( 22, GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,  OK )
 
C
C     CASE 3
C
      CALL TCASE ( 'Checking to make sure we can load a kernel '
     .//           'and add new ID/NAME pairs. ' )
 
 
      TEXT(  1 ) = BEGDAT()
      TEXT(  2 ) = ' NAIF_BODY_CODE  = ( 22, 23, 24, 25 )'
      TEXT(  3 ) = ' NAIF_BODY_NAME  = ( ''SPUD'', ''MOON'''
     .   //           ', ''CURLEY'', ''SHEMP'' )'
 
      CALL TSTTXT ( 'sample.nam', TEXT, 3, .TRUE., .FALSE.   )
 
      CALL BODN2C ( 'SPUD', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 22, 0, OK )
 
      CALL BODC2N ( 24, GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,           OK )
      CALL CHCKSC ( 'GET'  , GET  , '='   , 'CURLEY', OK )
 
C
C     CASE 4
C
C     Test to check we can override a name-ID assignment from the
C     default list with an external kernel assignment.
C 
      CALL TCASE ( 'Checking to make sure we can override an '
     .//           'existing NAME/ID pair. ' )
 
      CALL BODN2C ( 'MOON', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', 23, 0, OK )
 

C
C     CASE 5
C
C     Test on external name-ID assignments.
C
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
      
C
C     CASE 6
C
      CALL TCASE ( 'Checking that we cannot override an '
     .//           'existing kernel based NAME/ID pair.' ) 

      TEXT(1) = BEGDAT()
      TEXT(2) = 'NAIF_BODY_CODE =  ( -170101, -170102 )'
      TEXT(3) = 'NAIF_BODY_NAME =  ( ''1701-A'', ''1701-b'' )'
      TEXT(4) = 'NAIF_BODY_CODE += ( -170105 )'
      TEXT(5) = 'NAIF_BODY_NAME += ( ''1701-a'' )'
      TEXT(6) = ' '
 
      CALL KILFIL ( 'testdata.ker' )
      CALL TSTTXT ( 'testdata.ker', TEXT, 6, .FALSE., .TRUE.)
      
C
C     CASE 7
C
C     Test of BODDEF.
C
      CALL TCASE ( 'Check BODDEF functionality.' )

      CALL BODDEF ( 'Woof', -10009 )

      CALL BODC2N ( -10009, GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSC ( 'GET', GET, '=', 'Woof', OK )

C
C     Assign a member of the equivalence class to the ID code.
C     As last on the stack, a C2N lookup should return the
C     same member string.
C     
      CALL BODDEF ( 'woof', -10009 )
      CALL BODC2N ( -10009, GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSC ( 'GET', GET, '=', 'woof', OK )

C
C     Case and space insensitiviy
C
      CALL BODN2C ( ' WOOF ', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', -10009, 0, OK )


C
C     Functionality unaffected by the previous test's error state?
C     Try a BODDEF.
C
      CALL BODDEF ( ' WOof ', -10009 )
      CALL CHCKXC ( .FALSE., ' ',  OK )

C
C     Now perform a post error case and space insensitiviy test.
C
      CALL BODN2C ( '   wOOf ', CODE, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSI ( 'CODE', CODE, '=', -10009, 0, OK )


C
C     CASE 8
C
C     Test the mapping precedence behavior.
C
C     Assign a mapping via BODDEF. Note: 'Tis very important to call
C     CLPOOL an clear the kernel pool before this set of tests. The
C     previous tests left the pool in an error signalling state. 
C
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

C
C     CASE 9
C
C     Load a new mapping to the kernel pool using the previous ID value.
C     This mapping should override BODDEF.
C
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


C
C     CASE 10
C
C     Assign another name to the previous ID code. This mapping
C     should override all others.
C
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


C
C     CASE 11
C
C     Clear the kernel pool, test for BODDEF's first mapping for the
C     test ID.
C
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

 
C
C     CASE 12
C
C     Assign another mapping via BODDEF to override the first
C     BODDEF mapping.
C
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


C
C     CASE 13
C
      CALL TCASE ( 'Check precedence functionality. Masking.' )

      CALL CLPOOL ()

C
C     Assign a mapping via BODDEF, then remap the name to a new
C     number. 
C
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


C
C     CASE 14
C     
C
C     The kernel pool assignment should mask the BODDEF assignment
C     so a request for the name of body 20000 ought to fail.
C
      CALL TCASE ( 'Check precedence functionality. De-masking.' )

      GET = ' '
      CALL BODC2N ( 20000,   GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )

C
C     Destroy the kernel pool entries. The BODDEF assignment should now
C     respond; a request for body 20000 should succeed.
C
      CALL CLPOOL ( )
      GET = ' '
      CALL BODC2N ( 20000,   GET, FOUND )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,   OK )
      CALL CHCKSC ( 'GET', GET, '=', 'TestName', OK )


C
C     CASE 15
C
C     Set the kernel assignment using a different body ID than
C     previous tests. This should remask the BODDEF assignment.
C

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


C
C     CASE 16
C
C     Test watcher logic in ZZBODN2C: if ZZBODKER fails due
C     to a body-name assignment error, ZZBODN2C should exit.
C     Make the call twice to ensure correct handling of the
C     error.
C
      CALL TCASE ( 'ZZBODN2C: test CVPOOL handling for bad mapping ' )
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ',  OK )
 
      TEXT(  1 ) = BEGDAT()
      TEXT(  2 ) = ' NAIF_BODY_CODE  = ( 22, 23, 24, 25 )'
      TEXT(  3 ) = ' NAIF_BODY_NAME  = ( ''SPUD'', ''MOON'','
     .   //                            '  ''CURLEY'' )'
 
      CALL TSTTXT ( 'sample.nam', TEXT, 3, .TRUE., .FALSE.   )
 
      CALL ZZBODN2C ( 'MOON', CODE, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSIONS)',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )


      CALL ZZBODN2C ( 'MOON', CODE, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSIONS)',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )


C
C     CASE 17
C
C     Test watcher logic in ZZBODC2N: if ZZBODKER fails due
C     to a body-name assignment error, ZZBODC2N should exit.
C     Make the call twice to ensure correct handling of the
C     error.
C
      CALL TCASE ( 'ZZBODC2N: test CVPOOL handling for bad mapping ' )
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ',  OK )
 
      TEXT(  1 ) = BEGDAT()
      TEXT(  2 ) = ' NAIF_BODY_CODE  = ( 22, 23, 24, 25 )'
      TEXT(  3 ) = ' NAIF_BODY_NAME  = ( ''SPUD'', ''MOON'','
     .   //                            ' ''CURLEY'' )'
 
      CALL TSTTXT ( 'sample.nam', TEXT, 3, .TRUE., .FALSE.   )
 
      CALL ZZBODC2N ( 23, GET, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSIONS)',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )

      CALL ZZBODC2N ( 23, GET, FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSIONS)',  OK )
      CALL CHCKSL ( 'FOUND', FOUND, .FALSE.,   OK )




C
C     CASE 17
C
C     Test watcher logic in ZZBODKIK: if ZZBODKER fails due
C     to a body-name assignment error, ZZBODKIK should exit.
C     Since ZZBODKIK doesn't make direct use of the kernel
C     pool assignments, call ZZBODC2N to ensure that it calls
C     ZZBODKER if NODATA is .TRUE. upon entry.
C
C     Run the sequence of calls twice to ensure correct handling of the
C     error.
C
      CALL TCASE ( 'ZZBODKIK: test CVPOOL handling for bad mapping ' )
 
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ',  OK )
 
      TEXT(  1 ) = BEGDAT()
      TEXT(  2 ) = ' NAIF_BODY_CODE  = ( 22, 23, 24, 25 )'
      TEXT(  3 ) = ' NAIF_BODY_NAME  = ( ''SPUD'', ''MOON'','
     .   //                            ' ''CURLEY'' )'
 
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
