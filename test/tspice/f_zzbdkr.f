C$Procedure F_ZZBDKR ( Body Kernel Pool Initialization Test Family )
 
      SUBROUTINE F_ZZBDKR ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the kernel pool
C     name-code mapping processing software.
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
C     This routine exercises the conformance of the kernel pool name
C     to code mapping initialization with intended/designed behavior.
C
C     We need not perform any stress tests here, as F_BODCOD attempts
C     to provide these sorts of tests from the higher level interfaces.
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
C     B.V. Semenov    (JPL)
C     F.S. Turner     (JPL)
C
C$ Version
C
C-    TSPICE Version 2.0.0, 16-SEP-2013 (BVS)
C
C        Updated to test the hash-based version of ZZBODKER.
C
C-    TSPICE Version 1.0.0, 26-AUG-2002 (FST)
C
C
C-&

C
C     SPICELIB functions
C
 
C
C     Local Parameters
C
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 80 )
 
      INTEGER               BUFLEN
      PARAMETER           ( BUFLEN = 20 )
 
      INTEGER               TKBASE
      PARAMETER           ( TKBASE = 10000 )
 
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL =  5 )
 
C
C     Local Variables
C
      CHARACTER*(LINLEN)    BUFFER ( BUFLEN )
 
      INTEGER               NVALS
 
      CHARACTER*(MAXL)      KERNAM (          NROOM )
      CHARACTER*(MAXL)      KERNOR (          NROOM )
      INTEGER               KERCOD (          NROOM )
      INTEGER               KERSIZ
      LOGICAL               EXTKER 
      INTEGER               BNMLST (          NROOM )
      INTEGER               BNMPOL ( LBPOOL : NROOM )
      CHARACTER*(MAXL)      BNMNMS (          NROOM )
      INTEGER               BNMIDX (          NROOM )
      INTEGER               BIDLST (          NROOM )
      INTEGER               BIDPOL ( LBPOOL : NROOM )
      INTEGER               BIDIDS (          NROOM )
      INTEGER               BIDIDX (          NROOM )

      CHARACTER*(MAXL)      NAMES  ( MAXVAL )
      CHARACTER*(MAXL)      NORNAM ( MAXVAL )
      INTEGER               CODES  ( MAXVAL )

      LOGICAL               NAMFND ( MAXVAL )
      LOGICAL               CODFND ( MAXVAL )
      INTEGER               NAMIDX ( MAXVAL )
      INTEGER               CODIDX ( MAXVAL )

      INTEGER               I
      INTEGER               J

      CHARACTER*(LINLEN)    TITLE
      CHARACTER*(LINLEN)    LINE

      INTEGER               CASE

      LOGICAL               FOUND

C
C     Save everything.
C
      SAVE

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZBDKR' )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPICE(MISSINGKPV) exception' )
 
C
C     First, check behavior if neither of the two kernel pool
C     variables are defined.
C
      CALL CLPOOL ( )
 
      CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ, EXTKER,
     .                BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                BIDLST, BIDPOL, BIDIDS, BIDIDX  )
 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL CHCKSL ( 'EXTKER', EXTKER, .FALSE., OK )
 
C
C     Now load up NAIF_BODY_CODE and call ZZBODKER.
C
      BUFFER(  1 ) = 'NAIF_BODY_CODE = 1000'
 
      CALL LMPOOL ( BUFFER, 1 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )

      EXTKER = .TRUE.
 
      CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ, EXTKER,
     .                BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                BIDLST, BIDPOL, BIDIDS, BIDIDX  )
 
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGKPV)', OK )
 
      CALL CHCKSL ( 'EXTKER', EXTKER, .FALSE., OK )
 
C
C     Now try NAIF_BODY_NAME and call ZZBODKER.
C
      CALL CLPOOL ( )
 
      BUFFER(  1 ) = 'NAIF_BODY_NAME = ''TEST_BODY_ZZBODKER'''
 
      CALL LMPOOL ( BUFFER, 1 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )

      EXTKER = .TRUE.
 
      CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ, EXTKER,
     .                BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                BIDLST, BIDPOL, BIDIDS, BIDIDX  )
 
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGKPV)', OK )
 
      CALL CHCKSL ( 'EXTKER', EXTKER, .FALSE., OK )
 
C
C     Clean up
C
      CALL CLPOOL ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPICE(KERVARTOOBIG) exception' )
 
C
C     Build up the kernel pool variables to exceed the available
C     space.  Start by overflowing NAIF_BODY_NAME.
C
      DO I = 1, NROOM
 
         BUFFER(  1 ) = 'NAIF_BODY_CODE += #'
         BUFFER(  2 ) = 'NAIF_BODY_NAME += ''NAME_#'''
 
         CALL REPMI ( BUFFER(1), '#', TKBASE+I, BUFFER(1) )
         CALL REPMI ( BUFFER(2), '#', TKBASE+I, BUFFER(2) )
 
         CALL LMPOOL ( BUFFER, 2 )
 
      END DO
 
      BUFFER(  1 ) = 'NAIF_BODY_CODE += #'
 
      CALL REPMI ( BUFFER(1), '#', TKBASE+NROOM+1, BUFFER(1) )
 
      CALL LMPOOL ( BUFFER, 1 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Invoke ZZBODKER and check for the exception.
C
      EXTKER = .TRUE.
 
      CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ, EXTKER,
     .                BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                BIDLST, BIDPOL, BIDIDS, BIDIDX  )
 
      CALL CHCKXC ( .TRUE., 'SPICE(KERVARTOOBIG)', OK )
 
      CALL CHCKSL ( 'EXTKER', EXTKER, .FALSE., OK )
 
      CALL CLPOOL ( )
 
C
C     Build up the kernel pool variables to exceed the available
C     space.  Overflow NAIF_BODY_CODE.

      DO I = 1, NROOM
 
         BUFFER(  1 ) = 'NAIF_BODY_CODE += #'
         BUFFER(  2 ) = 'NAIF_BODY_NAME += ''NAME_#'''
 
         CALL REPMI ( BUFFER(1), '#', TKBASE+I, BUFFER(1) )
         CALL REPMI ( BUFFER(2), '#', TKBASE+I, BUFFER(2) )
 
         CALL LMPOOL ( BUFFER, 2 )
 
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO
 
      BUFFER(  1 ) = 'NAIF_BODY_NAME += ''NAME_#'''
 
      CALL REPMI ( BUFFER(1), '#', TKBASE+NROOM+1, BUFFER(1) )
 
      CALL LMPOOL ( BUFFER, 1 )

      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Invoke ZZBODKER and check for the exception.
C
      EXTKER = .TRUE.
 
      CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ, EXTKER,
     .                BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                BIDLST, BIDPOL, BIDIDS, BIDIDX  )
 
      CALL CHCKXC ( .TRUE., 'SPICE(KERVARTOOBIG)', OK )
 
      CALL CHCKSL ( 'EXTKER', EXTKER, .FALSE., OK )
 
      CALL CLPOOL ( )
 
C
C     Build up the kernel pool variables to exceed the available
C     space.  Overflow both keywords.

      DO I = 1, NROOM + 1
 
         BUFFER(  1 ) = 'NAIF_BODY_CODE += #'
         BUFFER(  2 ) = 'NAIF_BODY_NAME += ''NAME_#'''
 
         CALL REPMI ( BUFFER(1), '#', TKBASE+I, BUFFER(1) )
         CALL REPMI ( BUFFER(2), '#', TKBASE+I, BUFFER(2) )
 
         CALL LMPOOL ( BUFFER, 2 )
 
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO
 
C
C     Invoke ZZBODKER and check for the exception.
C
      EXTKER = .TRUE.
 
      CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ, EXTKER,
     .                BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                BIDLST, BIDPOL, BIDIDS, BIDIDX  )
 
      CALL CHCKXC ( .TRUE., 'SPICE(KERVARTOOBIG)', OK )
 
      CALL CHCKSL ( 'EXTKER', EXTKER, .FALSE., OK )
 
      CALL CLPOOL ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPICE(BADDIMENSIONS) exception' )
 
C
C     To hit this, we have to have mismatched kernel pool
C     arrays with cardinality less than NROOM.
C
      BUFFER(  1 ) = 'NAIF_BODY_CODE += 10001'
      BUFFER(  2 ) = 'NAIF_BODY_NAME += ''TEST_BODY_ZZBODKER'''
      BUFFER(  3 ) = 'NAIF_BODY_CODE += 10002'
 
      CALL LMPOOL ( BUFFER, 3 )
 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Invoke ZZBODKER and check for the exception.
C
      EXTKER = .TRUE.
 
      CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ, EXTKER,
     .                BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                BIDLST, BIDPOL, BIDIDS, BIDIDX  )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADDIMENSIONS)', OK )
 
      CALL CHCKSL ( 'EXTKER', EXTKER, .FALSE., OK )
 
      CALL CLPOOL ( )
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'SPICE(BLANKNAMEASSIGNED) exception' )
 
C
C     To hit this exception, we need only assign a blank name
C     to a code.
C
      BUFFER(  1 ) = 'NAIF_BODY_CODE += 1000'
      BUFFER(  2 ) = 'NAIF_BODY_NAME += '' '''
 
      CALL LMPOOL ( BUFFER, 2 )

      CALL CHCKXC ( .FALSE., ' ', OK )
 
C
C     Invoke ZZBODKER and check for the exception.
C
      EXTKER = .TRUE.
 
      CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ, EXTKER,
     .                BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                BIDLST, BIDPOL, BIDIDS, BIDIDX  )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BLANKNAMEASSIGNED)', OK )
 
      CALL CHCKSL ( 'EXTKER', EXTKER, .FALSE., OK )


C
C --- Case: ------------------------------------------------------
C

C
C     Success cases.
C
      DO CASE = 1, 5 

C
C        Force-clear lists and hashes.
C
         DO I = 1, MAXVAL
            NAMFND (I) = .TRUE.
            CODFND (I) = .TRUE.
         END DO
         
         CALL CLEARC ( MAXVAL, NAMES  )
         CALL CLEARC ( MAXVAL, NORNAM )
         CALL CLEARI ( MAXVAL, CODES  )
         
         CALL CLEARI ( MAXVAL, NAMIDX )
         CALL CLEARI ( MAXVAL, CODIDX )
         
         CALL CLEARC ( NROOM, KERNAM )
         CALL CLEARC ( NROOM, KERNOR )
         CALL CLEARI ( NROOM, KERCOD )
         CALL CLEARI ( NROOM, BNMLST )
         CALL CLEARI ( NROOM, BNMPOL )
         CALL CLEARC ( NROOM, BNMNMS )
         CALL CLEARI ( NROOM, BNMIDX )
         CALL CLEARI ( NROOM, BIDLST )
         CALL CLEARI ( NROOM, BIDPOL )
         CALL CLEARI ( NROOM, BIDIDS )
         CALL CLEARI ( NROOM, BIDIDX )

         EXTKER = .FALSE.

C
C        Set up inputs and expected values for each test case.
C
         IF ( CASE .EQ. 1 ) THEN

C
C           This is simple mapping with a few distinct names mapping
C           to a few distinct IDs.
C
            TITLE = 'Simple one-to-one mapping'
      
            NVALS      = 3
            
            NAMES  (1) = ' Alpha'
            NORNAM (1) = 'ALPHA'
            CODES  (1) = 1001
            NAMFND (1) = .TRUE.
            CODFND (1) = .TRUE.
            NAMIDX (1) = 1
            CODIDX (1) = 1
            
            NAMES  (2) = '    BraVo'
            NORNAM (2) = 'BRAVO'
            CODES  (2) = 1002
            NAMFND (2) = .TRUE.
            CODFND (2) = .TRUE.
            NAMIDX (2) = 2
            CODIDX (2) = 2
            
            NAMES  (3) = '  Charlie'
            NORNAM (3) = 'CHARLIE'
            CODES  (3) = 1003
            NAMFND (3) = .TRUE.
            CODFND (3) = .TRUE.
            NAMIDX (3) = 3
            CODIDX (3) = 3
            
            NAMES  (4) = '   Delta'
            NORNAM (4) = 'DELTA'
            CODES  (4) = 1004
            NAMFND (4) = .FALSE.
            CODFND (4) = .FALSE.
            NAMIDX (4) = 0
            CODIDX (4) = 0
            
            NAMES  (5) = '     eCHo'
            NORNAM (5) = 'ECHO'
            CODES  (5) = 1005
            NAMFND (5) = .FALSE.
            CODFND (5) = .FALSE.
            NAMIDX (5) = 0
            CODIDX (5) = 0

         ELSE IF ( CASE .EQ. 2 ) THEN

C
C           This mapping has a few duplicate pairs.
C
            TITLE = 'Mapping with duplicates'
      
            NVALS      = 4
            
            NAMES  (1) = '    BraVo'
            NORNAM (1) = 'BRAVO'
            CODES  (1) = 1002
            NAMFND (1) = .TRUE.
            CODFND (1) = .TRUE.
            NAMIDX (1) = 2
            CODIDX (1) = 2
            
            NAMES  (2) = '    BraVo'
            NORNAM (2) = 'BRAVO'
            CODES  (2) = 1002
            NAMFND (2) = .TRUE.
            CODFND (2) = .TRUE.
            NAMIDX (2) = 2
            CODIDX (2) = 2
            
            NAMES  (3) = '  Charlie'
            NORNAM (3) = 'CHARLIE'
            CODES  (3) = 1003
            NAMFND (3) = .TRUE.
            CODFND (3) = .TRUE.
            NAMIDX (3) = 4
            CODIDX (3) = 4
            
            NAMES  (4) = '  Charlie'
            NORNAM (4) = 'CHARLIE'
            CODES  (4) = 1003
            NAMFND (4) = .TRUE.
            CODFND (4) = .TRUE.
            NAMIDX (4) = 4
            CODIDX (4) = 4
            
            NAMES  (5) = '     eCHo'
            NORNAM (5) = 'ECHO'
            CODES  (5) = 1005
            NAMFND (5) = .FALSE.
            CODFND (5) = .FALSE.
            NAMIDX (5) = 0
            CODIDX (5) = 0

         ELSE IF ( CASE .EQ. 3 ) THEN

C
C           This mapping has a few distinct names mapping to the
C           same ID.
C
            TITLE = 'Mapping with different names and same ID'
      
            NVALS      = 3
            
            NAMES  (1) = ' Alpha'
            NORNAM (1) = 'ALPHA'
            CODES  (1) = 1003
            NAMFND (1) = .TRUE.
            CODFND (1) = .TRUE.
            NAMIDX (1) = 1
            CODIDX (1) = 3
            
            NAMES  (2) = '    BraVo'
            NORNAM (2) = 'BRAVO'
            CODES  (2) = 1003
            NAMFND (2) = .TRUE.
            CODFND (2) = .TRUE.
            NAMIDX (2) = 2
            CODIDX (2) = 3
            
            NAMES  (3) = '  Charlie'
            NORNAM (3) = 'CHARLIE'
            CODES  (3) = 1003
            NAMFND (3) = .TRUE.
            CODFND (3) = .TRUE.
            NAMIDX (3) = 3
            CODIDX (3) = 3
            
            NAMES  (4) = '   Delta'
            NORNAM (4) = 'DELTA'
            CODES  (4) = 1004
            NAMFND (4) = .FALSE.
            CODFND (4) = .FALSE.
            NAMIDX (4) = 0
            CODIDX (4) = 0
            
            NAMES  (5) = '     eCHo'
            NORNAM (5) = 'ECHO'
            CODES  (5) = 1005
            NAMFND (5) = .FALSE.
            CODFND (5) = .FALSE.
            NAMIDX (5) = 0
            CODIDX (5) = 0

         ELSE IF ( CASE .EQ. 4 ) THEN

C
C           This mapping has the single name mapping to a few different
C           IDs, all of which but the last should be masked.
C
            TITLE = 'Mapping with same name different (masked) ID'
      
            NVALS      = 5
            
            NAMES  (1) = '    BraVo'
            NORNAM (1) = 'BRAVO'
            CODES  (1) = 1001
            NAMFND (1) = .TRUE.
            CODFND (1) = .FALSE.
            NAMIDX (1) = 2
            CODIDX (1) = 0
            
            NAMES  (2) = '    BraVo'
            NORNAM (2) = 'BRAVO'
            CODES  (2) = 1002
            NAMFND (2) = .TRUE.
            CODFND (2) = .TRUE.
            NAMIDX (2) = 2
            CODIDX (2) = 2
            
            NAMES  (3) = '  Charlie'
            NORNAM (3) = 'CHARLIE'
            CODES  (3) = 1003
            NAMFND (3) = .TRUE.
            CODFND (3) = .FALSE.
            NAMIDX (3) = 5
            CODIDX (3) = 0
            
            NAMES  (4) = '  Charlie'
            NORNAM (4) = 'CHARLIE'
            CODES  (4) = 1004
            NAMFND (4) = .TRUE.
            CODFND (4) = .FALSE.
            NAMIDX (4) = 5
            CODIDX (4) = 0
            
            NAMES  (5) = '  Charlie'
            NORNAM (5) = 'CHARLIE'
            CODES  (5) = 1005
            NAMFND (5) = .TRUE.
            CODFND (5) = .TRUE.
            NAMIDX (5) = 5
            CODIDX (5) = 5

         ELSE IF ( CASE .EQ. 5 ) THEN

C
C           This is a mixed case with a few distinct names mapping to
C           the same ID and a single name mapping for more than one ID.
C
            TITLE = 'Mixed mapping'
      
            NVALS      = 4
            
            NAMES  (1) = ' Alpha'
            NORNAM (1) = 'ALPHA'
            CODES  (1) = 1002
            NAMFND (1) = .TRUE.
            CODFND (1) = .TRUE.
            NAMIDX (1) = 1
            CODIDX (1) = 2
            
            NAMES  (2) = '    BraVo'
            NORNAM (2) = 'BRAVO'
            CODES  (2) = 1002
            NAMFND (2) = .TRUE.
            CODFND (2) = .TRUE.
            NAMIDX (2) = 2
            CODIDX (2) = 2
            
            NAMES  (3) = '  Charlie'
            NORNAM (3) = 'CHARLIE'
            CODES  (3) = 1003
            NAMFND (3) = .TRUE.
            CODFND (3) = .FALSE.
            NAMIDX (3) = 4
            CODIDX (3) = 0
            
            NAMES  (4) = '  Charlie'
            NORNAM (4) = 'CHARLIE'
            CODES  (4) = 1004
            NAMFND (4) = .TRUE.
            CODFND (4) = .TRUE.
            NAMIDX (4) = 4
            CODIDX (4) = 4
            
            NAMES  (5) = '     eCHo'
            NORNAM (5) = 'ECHO'
            CODES  (5) = 1005
            NAMFND (5) = .FALSE.
            CODFND (5) = .FALSE.
            NAMIDX (5) = 0
            CODIDX (5) = 0

         END IF

C
C        Report case title.
C 
         CALL TCASE ( TITLE )

C
C        Clear POOL.
C
         CALL CLPOOL ( )

C
C        Package values into POOL variables and put them in the POOL.
C         
         DO I = 1, NVALS

            BUFFER(1) = 'NAIF_BODY_CODE += #'
            BUFFER(2) = 'NAIF_BODY_NAME += '''

            CALL REPMI ( BUFFER(1), '#', CODES(I), BUFFER( 1 ) )

            CALL SUFFIX ( NAMES(I), 0, BUFFER(2) )
            CALL SUFFIX ( '''', 0, BUFFER(2) )

            CALL LMPOOL ( BUFFER, 2 )

         END DO

C
C        Toss inputs at ZZBODINI.
C
         CALL ZZBODKER ( KERNAM, KERNOR, KERCOD, KERSIZ, EXTKER,
     .                   BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                   BIDLST, BIDPOL, BIDIDS, BIDIDX  )
         
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check if lists contain what we expect.
C
         CALL CHCKSL ( 'EXTKER', EXTKER, .TRUE., OK )

         CALL CHCKSI ( 'KERSIZ', KERSIZ, '=', NVALS,  0,     OK )
        
         CALL CHCKAC ( 'KERNAM', KERNAM, '=', NAMES,  NVALS, OK )
         CALL CHCKAC ( 'KERNOR', KERNOR, '=', NORNAM, NVALS, OK )

         CALL CHCKAI ( 'KERCOD', KERCOD, '=', CODES,  NVALS, OK )

C
C        Check hashes to see if they contain what we expect.
C
         DO I = 1, MAXVAL 

C
C           Check name hash first.
C
            CALL ZZHSCCHK ( BNMLST, BNMPOL, BNMNMS, NORNAM(I), J )

            FOUND = J .NE. 0

            LINE = 'Name # found in hash'
            CALL REPMC ( LINE, '#', NORNAM(I), LINE )
            CALL CHCKSL ( LINE, FOUND, NAMFND(I), OK )

            IF ( J .NE. 0 ) THEN
               CALL CHCKSI ( 'BNMIDX(J)', BNMIDX(J), '=', 
     .                                              NAMIDX(I), 0, OK )
            END IF

C
C           Now check ID hash.
C
            CALL ZZHSICHK ( BIDLST, BIDPOL, BIDIDS, CODES(I), J )

            FOUND = J .NE. 0

            LINE = 'CODE # found in hash'
            CALL REPMI ( LINE, '#', CODES(I), LINE )
            CALL CHCKSL ( LINE, FOUND, CODFND(I), OK )

            IF ( J .NE. 0 ) THEN
               CALL CHCKSI ( 'BIDIDX(J)', BIDIDX(J), '=', 
     .                                              CODIDX(I), 0, OK )
            END IF

         END DO

      END DO

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
 
      END
