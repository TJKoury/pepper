C$Procedure F_ZZBDIN ( Body Name/Code Initialization Test Family )
 
      SUBROUTINE F_ZZBDIN ( OK )
 
C$ Abstract
C
C     Test family to exercise the logic and code in the body name-code
C     processing software.
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
C     This routine exercise the conformance of the body name-code
C     initialization routine, ZZBODINI, to the intended/designed
C     behavior.
C
C     We need not perform any stress tests here, as F_BODCOD
C     attempts to provide these sorts of tests from higher level
C     interfaces.
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
C        Updated to test the hash-based version of ZZBODINI.
C
C-    TSPICE Version 1.0.0, 26-AUG-2002 (FST)
C
C
C-&

C
C     Local parameters.
C
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL =  5 )
 
C
C     Local Variables
C
      CHARACTER*(MAXL)      NAMES  ( MAXVAL )
      CHARACTER*(MAXL)      NORNAM ( MAXVAL )
      INTEGER               CODES  ( MAXVAL )
      INTEGER               NVALS

      INTEGER               BNMLST (          MAXVAL )
      INTEGER               BNMPOL ( LBPOOL : MAXVAL )
      CHARACTER*(MAXL)      BNMNMS (          MAXVAL )
      INTEGER               BNMIDX (          MAXVAL )

      INTEGER               BIDLST (          MAXVAL )
      INTEGER               BIDPOL ( LBPOOL : MAXVAL )
      INTEGER               BIDIDS (          MAXVAL )
      INTEGER               BIDIDX (          MAXVAL )

      LOGICAL               NAMFND ( MAXVAL )
      LOGICAL               CODFND ( MAXVAL )
      INTEGER               NAMIDX ( MAXVAL )
      INTEGER               CODIDX ( MAXVAL )

      INTEGER               I
      INTEGER               J

      CHARACTER*(80)        TITLE
      CHARACTER*(80)        LINE

      INTEGER               CASE

      LOGICAL               FOUND

C
C     Save everything.
C
      SAVE
 
C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZBDIN' )

C
C     First check the only ZZBODINI exception that we can trigger.
C
      CALL TCASE ( 'Exception: inconsistent dimensions.' )

      CALL ZZBODINI ( NAMES,  NORNAM, CODES,  MAXVAL+1, MAXVAL,
     .                BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                BIDLST, BIDPOL, BIDIDS, BIDIDX  )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BUG1)', OK ) 
 
C
C     Then check success cases.
C
      DO CASE = 1, 5 

C
C        Force-clear inputs and hashes.
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
         
         CALL CLEARI ( MAXVAL, BNMLST )
         CALL CLEARI ( MAXVAL, BNMPOL )
         CALL CLEARC ( MAXVAL, BNMNMS )
         CALL CLEARI ( MAXVAL, BNMIDX )
         CALL CLEARI ( MAXVAL, BIDLST )
         CALL CLEARI ( MAXVAL, BIDPOL )
         CALL CLEARI ( MAXVAL, BIDIDS )
         CALL CLEARI ( MAXVAL, BIDIDX )

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
C        Toss inputs at ZZBODINI.
C
         CALL ZZBODINI ( NAMES,  NORNAM, CODES,  NVALS, MAXVAL,
     .                   BNMLST, BNMPOL, BNMNMS, BNMIDX,
     .                   BIDLST, BIDPOL, BIDIDS, BIDIDX  )
         
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check if we got back what we expected.
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
