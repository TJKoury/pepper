C$Procedure F_ZZHSI ( Family of tests for ZZHSI )
 
      SUBROUTINE F_ZZHSI ( OK )

      IMPLICIT NONE
 
C$ Abstract
C
C     This routine tests add-only integer hashes.
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
C   None
C
C$ Keywords
C
C   None
C
C$ Declarations

      INCLUDE              'zzbodtrn.inc'

      LOGICAL               OK

C$ Brief_I/O
C
C   None
C
C$ Detailed_Input
C
C   None
C
C$ Detailed_Output
C
C   None
C
C$ Parameters
C
C   None
C
C$ Exceptions
C
C   None
C
C$ Files
C
C   None
C
C$ Particulars
C
C   None
C
C$ Examples
C
C   None
C
C$ Restrictions
C
C   None
C
C$ Literature_References
C
C   None
C
C$ Author_and_Institution
C
C   None
C
C$ Version
C
C-    Version 1.1.0  01-FEB-2016 (BVS)
C
C        Updated LONGEST LIST SIZE in test case 3 (4 -> 5).
C
C-    Version 1.0.0  28-FEB-2014 (BVS)
C
C-&
 
C$ Index_Entries
C
C   None
C
C-&

C
C     Test Utility Functions
C

C
C     SPICELIB Functions
C
C      DOUBLE PRECISION      T_RANDD
      INTEGER               CARDI
      INTEGER               INTMAX
      INTEGER               ZZHASHI

C
C     Local Parameters
C
      INTEGER               SIZIDX
      PARAMETER           ( SIZIDX =   0 )
 
      INTEGER               FREIDX
      PARAMETER           ( FREIDX =  -1 )

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               HASHSZ
C      PARAMETER           ( HASHSZ = 26003 )
      PARAMETER           ( HASHSZ = 5003 )

C
C     Local Variables
C
      INTEGER               HEDLST  (          HASHSZ )
      INTEGER               COLLST  ( LBPOOL : HASHSZ )
      INTEGER               ITEMS   (          HASHSZ )
      INTEGER               ITEMAT
      LOGICAL               NEW
 
      INTEGER               TITEMS  (          HASHSZ )
      INTEGER               IDS     (          HASHSZ )
      INTEGER               PIDS    (          HASHSZ )

      INTEGER               HSHVAL  ( LBCELL : HASHSZ )
      INTEGER               EXPUSD
      INTEGER               EXPFRE

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               HINT

C      INTEGER               SEED

      INTEGER               AVAIL

      INTEGER               BLTCOD  ( NPERM )
      CHARACTER*(MAXL)      BLTNAM  ( NPERM )

      LOGICAL               REPORT
 
C
C     Set timing reporting. Set to .FALSE. to suppress screen output.
C
C      REPORT = .TRUE.
      REPORT = .FALSE.

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZHSI' )
 
C
C     Initialize items and IDs - random values.
C
C      SEED = -1
C
C      DO I = 1, HASHSZ
C
C         IDS(I) = I * 1000
C
C         TITEMS(I) = NINT( T_RANDD( 1.D0, 9.D0, SEED ) ) + 10 * I
C
C         IF ( MOD( TITEMS(I), 2 ) .EQ. 0 ) THEN
C            TITEMS(I) = -TITEMS(I)
C         END IF
C
C      END DO

C
C     Initialize items and IDs -- NAIF ID like values.
C
C     First save all unique IDs built into the toolkit.
C
      CALL ZZIDMAP( BLTCOD, BLTNAM )

      I = 1
      J = 1

      DO WHILE ( I .LE. NPERM .AND. J .LE. HASHSZ )
         IF ( I .GT. 1 ) THEN
            IF ( BLTCOD(I) .NE. BLTCOD(I-1) ) THEN
               TITEMS(J) = BLTCOD(I)
               J = J + 1
            END IF
         ELSE
            TITEMS(J) = BLTCOD(I)
            J = J + 1
         END IF
         I = I + 1
      END DO

C
C     Then save 63 extra IDs based on each unique each s/c ID.
C
      IF ( J .LT. HASHSZ ) THEN
         DO I = 2, NPERM
            IF ( BLTCOD(I) .LT. 0           .AND. 
     .           BLTCOD(I) .NE. BLTCOD(I-1)       ) THEN
               DO K = 1, 9
                  IF ( J .LE. HASHSZ ) THEN
                     TITEMS(J) = BLTCOD(I) * 1000 - K * 100
                     J = J + 1
                  END IF
                  IF ( J .LE. HASHSZ ) THEN
                     TITEMS(J) = BLTCOD(I) * 1000 - K * 100 - 1
                     J = J + 1
                  END IF
                  IF ( J .LE. HASHSZ ) THEN
                     TITEMS(J) = BLTCOD(I) * 1000 - K * 100 - 2
                     J = J + 1
                  END IF
                  IF ( J .LE. HASHSZ ) THEN
                     TITEMS(J) = BLTCOD(I) * 1000 - K * 100 - 5
                     J = J + 1
                  END IF
                  IF ( J .LE. HASHSZ ) THEN
                     TITEMS(J) = BLTCOD(I) * 1000 - K * 100 - 10
                     J = J + 1
                  END IF
                  IF ( J .LE. HASHSZ ) THEN
                     TITEMS(J) = BLTCOD(I) * 1000 - K * 100 - 11
                     J = J + 1
                  END IF
                  IF ( J .LE. HASHSZ ) THEN
                     TITEMS(J) = BLTCOD(I) * 1000 - K * 100 - 12
                     J = J + 1
                  END IF
               END DO
            END IF
         END DO
      END IF

C
C     Then add some asteroid IDs at the end to fill up the hash.
C
      DO I = J, HASHSZ
         TITEMS(I) = 2500000 + I
      END DO

      DO I = 1, HASHSZ
         IDS(I) = I * 1000
      END DO

C
C     Calculate hash values for all IDs in the buffer and store them in
C     a set to get the expected numbers of occupied and free head
C     nodes.
C
      CALL SSIZEI ( HASHSZ, HSHVAL )
      DO I = 1, HASHSZ
         J = ZZHASHI( TITEMS(I), HASHSZ )
         CALL INSRTI ( J, HSHVAL )
      END DO

      EXPUSD = CARDI( HSHVAL )
      EXPFRE = HASHSZ - EXPUSD

C
C     Initialize hash.
C
      CALL TCASE ( 'Initialize hash.' )

      CALL ZZHSIINI ( HASHSZ, HEDLST, COLLST )
 
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'SIZE',  COLLST(SIZIDX), '=', HASHSZ, 0, OK )
      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=',      1, 0, OK )
      DO I = 1, HASHSZ
         CALL CHCKSI ( 'HEDLST(I)', HEDLST(I), '=', 0, 0, OK )
      END DO

C
C     Populate hash.
C
      CALL TCASE ( 'Populate hash.' )

      DO I = 1, HASHSZ

         CALL ZZHSIADD ( HEDLST,   COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT, NEW )

         CALL CHCKXC ( .FALSE., ' ',  OK ) 
         CALL CHCKSL ( 'NEW',   NEW,   .TRUE.,  OK )
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=',    I,      0, OK )
         CALL CHCKSI ( 'ITEMS(ITEMAT)', ITEMS(ITEMAT), '=', 
     .                                           TITEMS(I), 0, OK )
         CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', I+1, 0, OK )

         IF ( ITEMAT .EQ. I ) THEN
            PIDS(ITEMAT) = IDS(I)
         END IF

         CALL ZZHSIAVL ( COLLST, AVAIL )
         CALL CHCKSI ( 'AVAIL', AVAIL, '=', HASHSZ - I, 0, OK )

      END DO

C
C     Get information about hash.
C
      CALL TCASE ( 'Get info about the hash.' )

      CALL ZZHSIINF ( HEDLST, COLLST, ITEMS, 'HASH SIZE', I )
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'HASH SIZE', I, '=', HASHSZ, 0, OK )

      CALL ZZHSIINF ( HEDLST, COLLST, ITEMS, 'USED HEADNODE COUNT', I )
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'USED HEADNODE COUNT', I, '=', EXPUSD, 0, OK )

      CALL ZZHSIINF ( HEDLST, COLLST, ITEMS, 'UNUSED HEADNODE COUNT',I )
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'UNUSED HEADNODE COUNT', I, '=', EXPFRE, 0, OK )

      CALL ZZHSIINF ( HEDLST, COLLST, ITEMS, 'USED ITEM COUNT', I )
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'USED ITEM COUNT', I, '=', HASHSZ, 0, OK )

      CALL ZZHSIINF ( HEDLST, COLLST, ITEMS, 'UNUSED ITEM COUNT', I )
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'UNUSED ITEM COUNT', I, '=', 0, 0, OK )

C
C     Note that the expected value in the LONGEST LIST SIZE check below
C     depend on IDs in the built-in name/ID mapping, which is likely to
C     change in each new toolkit version. Such change may or may not
C     result in a different expected value. If it does, it's not an
C     error but it means that this check must be updated.
C
      CALL ZZHSIINF ( HEDLST, COLLST, ITEMS, 'LONGEST LIST SIZE', I )
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'LONGEST LIST SIZE', I, '=', 4, 0, OK )

      CALL ZZHSIINF ( HEDLST, COLLST, ITEMS, 'Hakuna-Matata!', I )
      CALL CHCKXC ( .TRUE., 'SPICE(ITEMNOTRECOGNIZED)',  OK ) 
      CALL CHCKSI ( ' ', I, '=', 0, 0, OK )

C
C     Add the same items again.
C
      CALL TCASE ( 'Add the same items again.' )

      DO I = HASHSZ, 1, -1

         CALL ZZHSIADD ( HEDLST,   COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT, NEW )

         CALL CHCKXC ( .FALSE., ' ',  OK ) 
         CALL CHCKSL ( 'NEW',   NEW,   .FALSE.,  OK )
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=',    I,      0, OK )
         CALL CHCKSI ( 'ITEMS(ITEMAT)', ITEMS(ITEMAT), '=', 
     .                                         TITEMS(I), 0, OK )
         CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )

         IF ( ITEMAT .EQ. I ) THEN
            CALL CHCKSI ( 'PIDS(ITEMAT)', PIDS(ITEMAT), '=', 
     .                                                 IDS(I), 0, OK )
         END IF

         CALL ZZHSIAVL ( COLLST, AVAIL )
         CALL CHCKSI ( 'AVAIL', AVAIL, '=', 0, 0, OK )

      END DO

C
C     Check that items are in the hash.
C
      CALL TCASE ( 'Check that items are in the hash.' )

      DO I = HASHSZ, 1, -1

         CALL ZZHSICHK ( HEDLST, COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT )

         CALL CHCKXC ( .FALSE., ' ',  OK ) 
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=',    I,      0, OK )
         CALL CHCKSI ( 'ITEMS(ITEMAT)', ITEMS(ITEMAT), '=', 
     .                                           TITEMS(I), 0, OK )
         CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )

         IF ( ITEMAT .EQ. I ) THEN
            CALL CHCKSI ( 'PIDS(ITEMAT)', PIDS(ITEMAT), '=', 
     .                                                 IDS(I), 0, OK )
         END IF

      END DO

C
C     Check that non-hashed items are not in the hash.
C
      CALL TCASE ( 'Check that other items are not in the hash.' )

      DO I = HASHSZ, 1, -1

         HINT = TITEMS(HASHSZ) + INTMAX()/2

         CALL ZZHSICHK ( HEDLST, COLLST, ITEMS,
     .                   HINT, ITEMAT )

         CALL CHCKXC ( .FALSE., ' ',  OK ) 
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=', 0, 0, OK )

      END DO

C
C     Exception: try to add one more item to the hash.
C
      CALL TCASE ( 'Exception: hash overflow.' )

      CALL ZZHSIADD ( HEDLST, COLLST, ITEMS,
     .                HINT,  ITEMAT, NEW )
      
      CALL CHCKXC ( .TRUE., 'SPICE(HASHISFULL)',  OK ) 
      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )
      CALL CHCKSI ( 'ITEMAT', ITEMAT, '=', 0, 0, OK )

C
C     Exception: reset hash with negative count.
C
      CALL TCASE ( 'Exception: reset with non-positive size.' )

      CALL ZZHSIINI ( -1, HEDLST, COLLST )
 
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK ) 
      CALL CHCKSI ( 'SIZE',  COLLST(SIZIDX), '=', HASHSZ, 0, OK )
      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )

C
C     Reset hash.
C
      CALL TCASE ( 'Reset hash.' )

      CALL ZZHSIINI ( HASHSZ-1, HEDLST, COLLST )
 
      CALL CHCKXC ( .FALSE., ' ',  OK ) 
      CALL CHCKSI ( 'SIZE',  COLLST(SIZIDX), '=', HASHSZ-1, 0, OK )
      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=',        1, 0, OK )
      DO I = 1, HASHSZ-1
         CALL CHCKSI ( 'HEDLST(I)', HEDLST(I), '=', 0, 0, OK )
      END DO

      CALL ZZHSIAVL ( COLLST, AVAIL )
      CALL CHCKSI ( 'AVAIL', AVAIL, '=', HASHSZ - 1, 0, OK )

C
C     Check that items are not in the empty hash.
C
      CALL TCASE ( 'Check that items are not in empty hash.' )

      DO I = 1, HASHSZ 

         CALL ZZHSICHK ( HEDLST, COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT )

         CALL CHCKXC ( .FALSE., ' ',  OK ) 
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=', 0, 0, OK )

      END DO

C
C     Timing test.
C
      CALL TCASE ( 'Timing test.' )

      CALL T_ELAPSD ( .FALSE., 'no report on first call', 'total' )

      CALL ZZHSIINI ( HASHSZ, HEDLST, COLLST )
 
      CALL T_ELAPSD ( REPORT, 'initializing hash', 'running' )

      DO I = 1, HASHSZ
         CALL ZZHSIADD ( HEDLST,   COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT, NEW )
      END DO

      CALL T_ELAPSD ( REPORT, 'populating hash', 'running' )

      DO I = HASHSZ, 1, -1
         CALL ZZHSIADD ( HEDLST,   COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT, NEW )
      END DO

      CALL T_ELAPSD ( REPORT, 'adding same items to hash', 'running' )

      DO I = HASHSZ, 1, -1
         CALL ZZHSICHK ( HEDLST, COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT )
      END DO

      CALL T_ELAPSD ( REPORT, 'checking presence in hash', 'running' )

      DO I = HASHSZ, 1, -1
         HINT = TITEMS(I)+TITEMS(HASHSZ)
         CALL ZZHSICHK ( HEDLST, COLLST, ITEMS,
     .                   HINT, ITEMAT )
      END DO

      CALL T_ELAPSD ( REPORT, 'checking items not in hash', 'running' )

      CALL ZZHSIINI ( HASHSZ-1, HEDLST, COLLST )

      CALL T_ELAPSD ( REPORT, 'resetting hash', 'running' )

      DO I = 1, HASHSZ 
         CALL ZZHSICHK ( HEDLST, COLLST, ITEMS,
     .                   TITEMS(I), ITEMAT )

      END DO

      CALL T_ELAPSD ( REPORT, 'checking items in empty hash', 'running')

C
C     This is good enough for now. 
C

      CALL T_SUCCESS ( OK )
      RETURN
 
      END
