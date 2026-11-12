!$Procedure F_ZZHSC ( Family of tests for ZZHSC )

      SUBROUTINE F_ZZHSC ( OK )
      use pepper_legacy
      use pepper_inc_zzbodtrn

      IMPLICIT NONE

! $ Abstract
!
!     This routine tests add-only character hashes.
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
!-    Version 1.1.0  05-NOV-2014 (BVS)
!
!        Removed test case trying to set hash size to INTMAX as with
!        the change to ZZHSCINI in N0067 it now causes a seg fault
!        while previously it was caught as an exception by ZZHASH2.
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
!      INTEGER               INTMAX

!
!     Local parameters
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

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
      CHARACTER*(WDSIZE)    ITEMS   (          HASHSZ )
      INTEGER               ITEMAT
      LOGICAL               NEW

      CHARACTER*(WDSIZE)    TITEMS  (          HASHSZ )
      INTEGER               IDS     (          HASHSZ )
      INTEGER               PIDS    (          HASHSZ )

      INTEGER               HSHVAL  ( LBCELL : HASHSZ )
      INTEGER               EXPUSD
      INTEGER               EXPFRE

      INTEGER               I
      INTEGER               J
      INTEGER               K
      CHARACTER*(WDSIZE)    HWORD

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
      CALL TOPEN ( 'F_ZZHSC' )


!
!     Initialize items and IDs -- random values.
!
!      SEED = -1
!
!      DO I = 1, HASHSZ
!
!         IDS(I) = I * 1000
!
!         TITEMS(I) = ' '
!
!         DO J = 1, NINT( T_RANDD( 2.D0, 20.D0, SEED ) )
!
!            K = NINT( T_RANDD( 2.D0, 8.D0, SEED ) )
!
!            CALL SUFFIX( CHAR( MOD( I*K, 94 ) + 33 ), 0, TITEMS(I) )
!
!         END DO
!
!         HWORD = '_#'
!         CALL REPMI ( HWORD, '#', I, HWORD )
!         CALL SUFFIX( HWORD, 0, TITEMS(I) )
!
!      END DO


!
!     Initialize items and IDs -- NAIF name like values.
!
!     First save all names built into the toolkit and a few
!     names based on each of them.
!
      CALL ZZIDMAP( BLTCOD, BLTNAM )

      J = 1
      DO I = 1, NPERM
         IF ( BLTCOD(I) .GT. 0 .AND. J .LE. ( HASHSZ - 9 ) ) THEN
            CALL LJUCRS ( 0, BLTNAM(I), TITEMS(J) )
            CALL REPMC ( 'IAU_#',    '#', TITEMS(J), TITEMS(J+1) )
            CALL REPMC ( '#_FIXED',  '#', TITEMS(J), TITEMS(J+2) )
            CALL REPMC ( '#_TOPO',   '#', TITEMS(J), TITEMS(J+3) )
            CALL REPMC ( '#_1_SITE', '#', TITEMS(J), TITEMS(J+4) )
            CALL REPMC ( '#_2_SITE', '#', TITEMS(J), TITEMS(J+5) )
            CALL REPMC ( '#_3_SITE', '#', TITEMS(J), TITEMS(J+6) )
            CALL REPMC ( '#_ASITE',  '#', TITEMS(J), TITEMS(J+7) )
            CALL REPMC ( '#_BSITE',  '#', TITEMS(J), TITEMS(J+8) )
            CALL REPMC ( '#_CSITE',  '#', TITEMS(J), TITEMS(J+9) )
            J = J + 10
         END IF
         IF ( BLTCOD(I) .LT. 0 .AND. J .LE. ( HASHSZ - 39 ) ) THEN
            CALL LJUCRS ( 0, BLTNAM(I), TITEMS(J) )
            DO K = 1, 9
               CALL REPMC ( '#_'//CHAR(ICHAR('A')+K-1)//'_'//'CAM', &
                            '#', TITEMS(J), TITEMS(J+K) )
            END DO
            DO K = 10, 19
               CALL REPMC ( '#_'//CHAR(ICHAR('A')+K-1)//'_'//'SPE', &
                            '#', TITEMS(J), TITEMS(J+K) )
            END DO
            DO K = 20, 29
               CALL REPMC ( '#_'//CHAR(ICHAR('A')+K-20)//'_'//'DETECT', &
                            '#', TITEMS(J), TITEMS(J+K) )
            END DO
            DO K = 30, 39
               CALL REPMC ( '#_'//CHAR(ICHAR('A')+K-20)//'_'//'SENSOR', &
                            '#', TITEMS(J), TITEMS(J+K) )
            END DO
            J = J + 40
         END IF
      END DO

!
!     Eliminate duplicate values.
!
      K = J - 1
      CALL RMDUPC ( K, TITEMS )
      J = K + 1

!
!     Finish by adding some asteroid names at the end to fill up the
!     hash.
!
      DO I = J, HASHSZ
         CALL REPMI ( 'ASTEROID_#', '#', 2500000+I, TITEMS(I) )
      END DO

      DO I = 1, HASHSZ
         IDS(I) = I * 1000
      END DO

!
!     Calculate hash values for all names in the buffer and store them
!     in a set to get the expected numbers of occupied and free head
!     nodes.
!
      CALL SSIZEI ( HASHSZ, HSHVAL )
      DO I = 1, HASHSZ
         J = ZZHASH2( TITEMS(I), HASHSZ )
         CALL INSRTI ( J, HSHVAL )
      END DO

      EXPUSD = CARDI( HSHVAL )
      EXPFRE = HASHSZ - EXPUSD

!
!     Initialize hash.
!
      CALL TCASE ( 'Initialize hash.' )

      CALL ZZHSCINI ( HASHSZ, HEDLST, COLLST )

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

         CALL ZZHSCADD ( HEDLST,   COLLST, ITEMS, &
                         TITEMS(I), ITEMAT, NEW )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'NEW',   NEW,   .TRUE.,  OK )
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=',    I,      0, OK )
         CALL CHCKSC ( 'ITEMS(ITEMAT)', ITEMS(ITEMAT), '=', &
                                                 TITEMS(I),  OK )
         CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', I+1, 0, OK )

         IF ( ITEMAT .EQ. I ) THEN
            PIDS(ITEMAT) = IDS(I)
         END IF

         CALL ZZHSCAVL ( COLLST, AVAIL )
         CALL CHCKSI ( 'AVAIL', AVAIL, '=', HASHSZ - I, 0, OK )

      END DO

!
!     Get information about hash.
!
      CALL TCASE ( 'Get info about the hash.' )

      CALL ZZHSCINF ( HEDLST, COLLST, ITEMS, 'HASH SIZE', I )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSI ( 'HASH SIZE', I, '=', HASHSZ, 0, OK )

      CALL ZZHSCINF ( HEDLST, COLLST, ITEMS, 'USED HEADNODE COUNT', I )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSI ( 'USED HEADNODE COUNT', I, '=', EXPUSD, 0, OK )

      CALL ZZHSCINF ( HEDLST, COLLST, ITEMS, 'UNUSED HEADNODE COUNT',I )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSI ( 'UNUSED HEADNODE COUNT', I, '=', EXPFRE, 0, OK )

      CALL ZZHSCINF ( HEDLST, COLLST, ITEMS, 'USED ITEM COUNT', I )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSI ( 'USED ITEM COUNT', I, '=', HASHSZ, 0, OK )

      CALL ZZHSCINF ( HEDLST, COLLST, ITEMS, 'UNUSED ITEM COUNT', I )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSI ( 'UNUSED ITEM COUNT', I, '=', 0, 0, OK )

      CALL ZZHSCINF ( HEDLST, COLLST, ITEMS, 'LONGEST LIST SIZE', I )
      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSI ( 'LONGEST LIST SIZE', I, '=', 6, 0, OK )

      CALL ZZHSCINF ( HEDLST, COLLST, ITEMS, 'Hakuna-Matata!', I )
      CALL CHCKXC ( .TRUE., 'SPICE(ITEMNOTRECOGNIZED)',  OK )
      CALL CHCKSI ( ' ', I, '=', 0, 0, OK )

!
!     Add the same items again.
!
      CALL TCASE ( 'Add the same items again.' )

      DO I = HASHSZ, 1, -1

         CALL ZZHSCADD ( HEDLST,   COLLST, ITEMS, &
                         TITEMS(I), ITEMAT, NEW )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSL ( 'NEW',   NEW,   .FALSE.,  OK )
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=',    I,      0, OK )
         CALL CHCKSC ( 'ITEMS(ITEMAT)', ITEMS(ITEMAT), '=', &
                                                 TITEMS(I),  OK )
         CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )

         IF ( ITEMAT .EQ. I ) THEN
            CALL CHCKSI ( 'PIDS(ITEMAT)', PIDS(ITEMAT), '=', &
                                                       IDS(I), 0, OK )
         END IF

         CALL ZZHSCAVL ( COLLST, AVAIL )
         CALL CHCKSI ( 'AVAIL', AVAIL, '=', 0, 0, OK )

      END DO

!
!     Check that items are in the hash.
!
      CALL TCASE ( 'Check that items are in the hash.' )

      DO I = HASHSZ, 1, -1

         CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS, &
                         TITEMS(I), ITEMAT )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=',    I,      0, OK )
         CALL CHCKSC ( 'ITEMS(ITEMAT)', ITEMS(ITEMAT), '=', &
                                                 TITEMS(I),  OK )
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

         HWORD = TITEMS(I)

         CALL PREFIX ( 'z', 0, HWORD )

         CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS, &
                         HWORD, ITEMAT )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=', 0, 0, OK )

      END DO

!
!     Exception: try to add one more item to the hash.
!
      CALL TCASE ( 'Exception: hash overflow.' )

      CALL ZZHSCADD ( HEDLST, COLLST, ITEMS, &
                      HWORD,  ITEMAT, NEW )

      CALL CHCKXC ( .TRUE., 'SPICE(HASHISFULL)',  OK )
      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )
      CALL CHCKSI ( 'ITEMAT', ITEMAT, '=', 0, 0, OK )

!
!     Exception: reset hash with negative count.
!
      CALL TCASE ( 'Exception: reset with non-positive size.' )

      CALL ZZHSCINI ( -1, HEDLST, COLLST )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK )
      CALL CHCKSI ( 'SIZE',  COLLST(SIZIDX), '=', HASHSZ, 0, OK )
      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )

!
!     This case worked prior to N0067 update to ZZHSCINI as it
!     was trapped by ZZHASH2 before an attempt to assign hash
!     elements beywond its size was made by the initialization loop,
!     causing a seg fault. It was disabled for N0067.
!
!     Exception: reset hash with very big count.
!
!      CALL TCASE ( 'Exception: reset with too large size.' )
!
!      CALL ZZHSCINI ( INTMAX(), HEDLST, COLLST )
!
!      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIVISOR)', OK )
!      CALL CHCKSI ( 'SIZE',  COLLST(SIZIDX), '=', HASHSZ, 0, OK )
!      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=', HASHSZ+1, 0, OK )

!
!     Reset hash.
!
      CALL TCASE ( 'Reset hash.' )

      CALL ZZHSCINI ( HASHSZ-1, HEDLST, COLLST )

      CALL CHCKXC ( .FALSE., ' ',  OK )
      CALL CHCKSI ( 'SIZE',  COLLST(SIZIDX), '=', HASHSZ-1, 0, OK )
      CALL CHCKSI ( 'FIRST', COLLST(FREIDX), '=',        1, 0, OK )
      DO I = 1, HASHSZ-1
         CALL CHCKSI ( 'HEDLST(I)', HEDLST(I), '=', 0, 0, OK )
      END DO

      CALL ZZHSCAVL ( COLLST, AVAIL )
      CALL CHCKSI  ( 'AVAIL', AVAIL, '=', HASHSZ-1, 0, OK )

!
!     Check that items are not in the empty hash.
!
      CALL TCASE ( 'Check that items are not in empty hash.' )

      DO I = 1, HASHSZ

         CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS, &
                         TITEMS(I), ITEMAT )

         CALL CHCKXC ( .FALSE., ' ',  OK )
         CALL CHCKSI ( 'ITEMAT', ITEMAT, '=', 0, 0, OK )

      END DO

!
!     Timing test.
!
      CALL TCASE ( 'Timing test.' )

      CALL T_ELAPSD ( .FALSE., 'no report on first call', 'running' )

      CALL ZZHSCINI ( HASHSZ, HEDLST, COLLST )

      CALL T_ELAPSD ( REPORT, 'initializing hash', 'running' )

      DO I = 1, HASHSZ
         CALL ZZHSCADD ( HEDLST,   COLLST, ITEMS, &
                         TITEMS(I), ITEMAT, NEW )
      END DO

      CALL T_ELAPSD ( REPORT, 'populating hash', 'running' )

      DO I = HASHSZ, 1, -1
         CALL ZZHSCADD ( HEDLST,   COLLST, ITEMS, &
                         TITEMS(I), ITEMAT, NEW )
      END DO

      CALL T_ELAPSD ( REPORT, 'adding same items to hash', 'running' )

      DO I = HASHSZ, 1, -1
         CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS, &
                         TITEMS(I), ITEMAT )
      END DO

      CALL T_ELAPSD ( REPORT, 'checking presence in hash', 'running' )

      DO I = HASHSZ, 1, -1
         HWORD = TITEMS(I)
         CALL SUFFIX ( 'z', 0, HWORD )
         CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS, &
                         HWORD, ITEMAT )
      END DO

      CALL T_ELAPSD ( REPORT, 'checking items not in hash', 'running' )

      CALL ZZHSCINI ( HASHSZ-1, HEDLST, COLLST )

      CALL T_ELAPSD ( REPORT, 'resetting hash', 'running' )

      DO I = 1, HASHSZ
         CALL ZZHSCCHK ( HEDLST, COLLST, ITEMS, &
                         TITEMS(I), ITEMAT )

      END DO

      CALL T_ELAPSD ( REPORT, 'checking items in empty hash', 'running')

!
!     This is good enough for now.
!

      CALL T_SUCCESS ( OK )
      RETURN

      END
