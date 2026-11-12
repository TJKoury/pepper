!$Procedure F_ZZBDIN ( Body Name/Code Initialization Test Family )

      SUBROUTINE F_ZZBDIN ( OK )
      use pepper_legacy
      use pepper_inc_zzbodtrn

! $ Abstract
!
!     Test family to exercise the logic and code in the body name-code
!     processing software.
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
!     This routine exercise the conformance of the body name-code
!     initialization routine, ZZBODINI, to the intended/designed
!     behavior.
!
!     We need not perform any stress tests here, as F_BODCOD
!     attempts to provide these sorts of tests from higher level
!     interfaces.
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
!     B.V. Semenov    (JPL)
!     F.S. Turner     (JPL)
!
! $ Version
!
!-    TSPICE Version 2.0.0, 16-SEP-2013 (BVS)
!
!        Updated to test the hash-based version of ZZBODINI.
!
!-    TSPICE Version 1.0.0, 26-AUG-2002 (FST)
!
!
!-&

!
!     Local parameters.
!
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               MAXVAL
      PARAMETER           ( MAXVAL =  5 )

!
!     Local Variables
!
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

!
!     Save everything.
!
      SAVE

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZBDIN' )

!
!     First check the only ZZBODINI exception that we can trigger.
!
      CALL TCASE ( 'Exception: inconsistent dimensions.' )

      CALL ZZBODINI ( NAMES,  NORNAM, CODES,  MAXVAL+1, MAXVAL, &
                      BNMLST, BNMPOL, BNMNMS, BNMIDX, &
                      BIDLST, BIDPOL, BIDIDS, BIDIDX  )

      CALL CHCKXC ( .TRUE., 'SPICE(BUG1)', OK )

!
!     Then check success cases.
!
      DO CASE = 1, 5

!
!        Force-clear inputs and hashes.
!
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

!
!        Set up inputs and expected values for each test case.
!
         IF ( CASE .EQ. 1 ) THEN

!
!           This is simple mapping with a few distinct names mapping
!           to a few distinct IDs.
!
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

!
!           This mapping has a few duplicate pairs.
!
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

!
!           This mapping has a few distinct names mapping to the
!           same ID.
!
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

!
!           This mapping has the single name mapping to a few different
!           IDs, all of which but the last should be masked.
!
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

!
!           This is a mixed case with a few distinct names mapping to
!           the same ID and a single name mapping for more than one ID.
!
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

!
!        Report case title.
!
         CALL TCASE ( TITLE )

!
!        Toss inputs at ZZBODINI.
!
         CALL ZZBODINI ( NAMES,  NORNAM, CODES,  NVALS, MAXVAL, &
                         BNMLST, BNMPOL, BNMNMS, BNMIDX, &
                         BIDLST, BIDPOL, BIDIDS, BIDIDX  )

         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check if we got back what we expected.
!
         DO I = 1, MAXVAL

!
!           Check name hash first.
!
            CALL ZZHSCCHK ( BNMLST, BNMPOL, BNMNMS, NORNAM(I), J )

            FOUND = J .NE. 0

            LINE = 'Name # found in hash'
            CALL REPMC ( LINE, '#', NORNAM(I), LINE )
            CALL CHCKSL ( LINE, FOUND, NAMFND(I), OK )

            IF ( J .NE. 0 ) THEN
               CALL CHCKSI ( 'BNMIDX(J)', BNMIDX(J), '=', &
                                                    NAMIDX(I), 0, OK )
            END IF

!
!           Now check ID hash.
!
            CALL ZZHSICHK ( BIDLST, BIDPOL, BIDIDS, CODES(I), J )

            FOUND = J .NE. 0

            LINE = 'CODE # found in hash'
            CALL REPMI ( LINE, '#', CODES(I), LINE )
            CALL CHCKSL ( LINE, FOUND, CODFND(I), OK )

            IF ( J .NE. 0 ) THEN
               CALL CHCKSI ( 'BIDIDX(J)', BIDIDX(J), '=', &
                                                    CODIDX(I), 0, OK )
            END IF

         END DO

      END DO

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN

      END
