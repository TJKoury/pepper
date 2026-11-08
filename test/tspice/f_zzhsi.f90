!$Procedure F_ZZHSI ( Family of tests for ZZHSI )

      SUBROUTINE F_ZZHSI ( OK )

      IMPLICIT NONE

! $ Abstract
!
!     This routine tests add-only integer hashes.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!   None
!
! $ Keywords
!
!   None
!
! $ Declarations

      INCLUDE              'zzbodtrn.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!   None
!
! $ Detailed_Input
!
!   None
!
! $ Detailed_Output
!
!   None
!
! $ Parameters
!
!   None
!
! $ Exceptions
!
!   None
!
! $ Files
!
!   None
!
! $ Particulars
!
!   None
!
! $ Examples
!
!   None
!
! $ Restrictions
!
!   None
!
! $ Literature_References
!
!   None
!
! $ Author_and_Institution
!
!   None
!
! $ Version
!
!-    Version 1.1.0  01-FEB-2016 (BVS)
!
!        Updated LONGEST LIST SIZE in test case 3 (4 -> 5).
!
!-    Version 1.0.0  28-FEB-2014 (BVS)
!
!-&

! $ Index_Entries
!
!   None
!
!-&

!
!     Test Utility Functions
!

!
!     SPICELIB Functions
!
!      DOUBLE PRECISION      T_RANDD
      INTEGER               CARDI
      INTEGER               INTMAX
      INTEGER               ZZHASHI

!
!     Local Parameters
!
      INTEGER               SIZIDX
      PARAMETER           ( SIZIDX =   0 )

      INTEGER               FREIDX
      PARAMETER           ( FREIDX =  -1 )

      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               HASHSZ
!      PARAMETER           ( HASHSZ = 26003 )
      PARAMETER           ( HASHSZ = 5003 )

!
!     Local Variables
!
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

!      INTEGER               SEED

      INTEGER               AVAIL

      INTEGER               BLTCOD  ( NPERM )
      CHARACTER*(MAXL)      BLTNAM  ( NPERM )

      LOGICAL               REPORT

!
!     Set timing reporting. Set to .FALSE. to suppress screen output.
!
!      REPORT = .TRUE.
      REPORT = .FALSE.

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZHSI' )

!
!     Initialize items and IDs - random values.
!
!      SEED = -1
!
!      DO I = 1, HASHSZ
!
!         IDS(I) = I * 1000
!
!         TITEMS(I) = NINT( T_RANDD( 1.D0, 9.D0, SEED ) ) + 10 * I
!
!         IF ( MOD( TITEMS(I), 2 ) .EQ. 0 ) THEN
!            TITEMS(I) = -TITEMS(I)
!         END IF
!
!      END DO

!
!     Initialize items and IDs -- NAIF ID like values.
!
!     First save all unique IDs built into the toolkit.
!
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

!
!     Then save 63 extra IDs based on each unique each s/c ID.
!
      IF ( J .LT. HASHSZ ) THEN
         DO I = 2, NPERM
            IF ( BLTCOD(I) .LT. 0           .AND. &
                 BLTCOD(I) .NE. BLTCOD(I-1)       ) THEN
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

!
!     Then add some asteroid IDs at the end to fill up the hash.
!
      DO I = J, HASHSZ
         TITEMS(I) = 2500000 + I
      END DO

      DO I = 1, HASHSZ
         IDS(I) = I * 1000
      END DO

!
!     Calculate hash values for all IDs in the buffer and store them in
!     a set to get the expected numbers of occupied and free head
!     nodes.
!
      CALL SSIZEI ( HASHSZ, HSHVAL )
      DO I = 1, HASHSZ
         J = ZZHASHI( TITEMS(I), HASHSZ )
         CALL INSRTI ( J, HSHVAL )
      END DO

      EXPUSD = CARDI( HSHVAL )
      EXPFRE = HASHSZ - EXPUSD

!
!     Initialize hash.
!
      CALL TCASE ( 'Initialize hash.' )

      CALL ZZHSIINI ( HASHSZ, HEDLST, COLLST )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSI ( 'SIZE',  COLLST(SIZIDX), '=', HASHSZ, 0, OK )
      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=',      1, 0, OK )
      DO I = 1, HASHSZ
         CALL CHCKSI ( 'HEDLST(I)', HEDLST(I), '=', 0, 0, OK )
      END DO

!
!     Populate hash.
!
      CALL TCASE ( 'Populate hash.' )

      DO I = 1, HASHSZ

         CALL ZZHSIADD ( HEDLST,   COLLST, ITEMS, &
                         TITEMS(I), ITEMAT, NEW )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'NEW',   NEW,   .TRUE.,  OK )
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=',    I,      0, OK )
         CALL CHCKSI ( 'ITEMS(ITEMAT)', ITEMS(ITEMAT), '=', &
                                                 TITEMS(I), 0, OK )
         CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', I+1, 0, OK )

         IF ( ITEMAT .EQ. I ) THEN
            PIDS(ITEMAT) = IDS(I)
         END IF

         CALL ZZHSIAVL ( COLLST, AVAIL )
         CALL CHCKSI ( 'AVAIL', AVAIL, '=', HASHSZ - I, 0, OK )

      END DO

!
!     Get information about hash.
!
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

!
!     Note that the expected value in the LONGEST LIST SIZE check below
!     depend on IDs in the built-in name/ID mapping, which is likely to
!     change in each new toolkit version. Such change may or may not
!     result in a different expected value. If it does, it's not an
!     error but it means that this check must be updated.
!
      CALL ZZHSIINF ( HEDLST, COLLST, ITEMS, 'LONGEST LIST SIZE', I )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSI ( 'LONGEST LIST SIZE', I, '=', 4, 0, OK )

      CALL ZZHSIINF ( HEDLST, COLLST, ITEMS, 'Hakuna-Matata!', I )
      CALL CHCKXC ( .TRUE., 'SPICE(ITEMNOTRECOGNIZED)',  OK )
      CALL CHCKSI ( ' ', I, '=', 0, 0, OK )

!
!     Add the same items again.
!
      CALL TCASE ( 'Add the same items again.' )

      DO I = HASHSZ, 1, -1

         CALL ZZHSIADD ( HEDLST,   COLLST, ITEMS, &
                         TITEMS(I), ITEMAT, NEW )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'NEW',   NEW,   .FALSE.,  OK )
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=',    I,      0, OK )
         CALL CHCKSI ( 'ITEMS(ITEMAT)', ITEMS(ITEMAT), '=', &
                                               TITEMS(I), 0, OK )
         CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )

         IF ( ITEMAT .EQ. I ) THEN
            CALL CHCKSI ( 'PIDS(ITEMAT)', PIDS(ITEMAT), '=', &
                                                       IDS(I), 0, OK )
         END IF

         CALL ZZHSIAVL ( COLLST, AVAIL )
         CALL CHCKSI ( 'AVAIL', AVAIL, '=', 0, 0, OK )

      END DO

!
!     Check that items are in the hash.
!
      CALL TCASE ( 'Check that items are in the hash.' )

      DO I = HASHSZ, 1, -1

         CALL ZZHSICHK ( HEDLST, COLLST, ITEMS, &
                         TITEMS(I), ITEMAT )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=',    I,      0, OK )
         CALL CHCKSI ( 'ITEMS(ITEMAT)', ITEMS(ITEMAT), '=', &
                                                 TITEMS(I), 0, OK )
         CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )

         IF ( ITEMAT .EQ. I ) THEN
            CALL CHCKSI ( 'PIDS(ITEMAT)', PIDS(ITEMAT), '=', &
                                                       IDS(I), 0, OK )
         END IF

      END DO

!
!     Check that non-hashed items are not in the hash.
!
      CALL TCASE ( 'Check that other items are not in the hash.' )

      DO I = HASHSZ, 1, -1

         HINT = TITEMS(HASHSZ) + INTMAX()/2

         CALL ZZHSICHK ( HEDLST, COLLST, ITEMS, &
                         HINT, ITEMAT )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=', 0, 0, OK )

      END DO

!
!     Exception: try to add one more item to the hash.
!
      CALL TCASE ( 'Exception: hash overflow.' )

      CALL ZZHSIADD ( HEDLST, COLLST, ITEMS, &
                      HINT,  ITEMAT, NEW )

      CALL CHCKXC ( .TRUE., 'SPICE(HASHISFULL)',  OK )
      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )
      CALL CHCKSI ( 'ITEMAT', ITEMAT, '=', 0, 0, OK )

!
!     Exception: reset hash with negative count.
!
      CALL TCASE ( 'Exception: reset with non-positive size.' )

      CALL ZZHSIINI ( -1, HEDLST, COLLST )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK )
      CALL CHCKSI ( 'SIZE',  COLLST(SIZIDX), '=', HASHSZ, 0, OK )
      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )

!
!     Reset hash.
!
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

!
!     Check that items are not in the empty hash.
!
      CALL TCASE ( 'Check that items are not in empty hash.' )

      DO I = 1, HASHSZ

         CALL ZZHSICHK ( HEDLST, COLLST, ITEMS, &
                         TITEMS(I), ITEMAT )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=', 0, 0, OK )

      END DO

!
!     Timing test.
!
      CALL TCASE ( 'Timing test.' )

      CALL T_ELAPSD ( .FALSE., 'no report on first call', 'total' )

      CALL ZZHSIINI ( HASHSZ, HEDLST, COLLST )

      CALL T_ELAPSD ( REPORT, 'initializing hash', 'running' )

      DO I = 1, HASHSZ
         CALL ZZHSIADD ( HEDLST,   COLLST, ITEMS, &
                         TITEMS(I), ITEMAT, NEW )
      END DO

      CALL T_ELAPSD ( REPORT, 'populating hash', 'running' )

      DO I = HASHSZ, 1, -1
         CALL ZZHSIADD ( HEDLST,   COLLST, ITEMS, &
                         TITEMS(I), ITEMAT, NEW )
      END DO

      CALL T_ELAPSD ( REPORT, 'adding same items to hash', 'running' )

      DO I = HASHSZ, 1, -1
         CALL ZZHSICHK ( HEDLST, COLLST, ITEMS, &
                         TITEMS(I), ITEMAT )
      END DO

      CALL T_ELAPSD ( REPORT, 'checking presence in hash', 'running' )

      DO I = HASHSZ, 1, -1
         HINT = TITEMS(I)+TITEMS(HASHSZ)
         CALL ZZHSICHK ( HEDLST, COLLST, ITEMS, &
                         HINT, ITEMAT )
      END DO

      CALL T_ELAPSD ( REPORT, 'checking items not in hash', 'running' )

      CALL ZZHSIINI ( HASHSZ-1, HEDLST, COLLST )

      CALL T_ELAPSD ( REPORT, 'resetting hash', 'running' )

      DO I = 1, HASHSZ
         CALL ZZHSICHK ( HEDLST, COLLST, ITEMS, &
                         TITEMS(I), ITEMAT )

      END DO

      CALL T_ELAPSD ( REPORT, 'checking items in empty hash', 'running')

!
!     This is good enough for now.
!

      CALL T_SUCCESS ( OK )
      RETURN

      END
